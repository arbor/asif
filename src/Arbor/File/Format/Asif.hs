{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Arbor.File.Format.Asif
  ( binarySearch
  , binarySearchExact
  , extractSegments
  , extractNamedSegments
  , getCodeMap
  , getNameMap
  , segmentCidrs
  , segmentCodes
  , segmentElements
  , segmentIdentifiers
  ) where

import Arbor.File.Format.Asif.ByteString.Builder
import Arbor.File.Format.Asif.Type
import Control.Lens
import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Builder
import Data.Either
import Data.Either.Combinators
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text                                 (Text)
import Data.Text.Encoding                        (decodeUtf8')
import Data.Thyme.Clock                          (microseconds)
import Data.Thyme.Clock.POSIX                    (POSIXTime)
import Data.Traversable
import Data.Word

import qualified Arbor.File.Format.Asif.Lens as L
import qualified Data.Attoparsec.ByteString  as AP
import qualified Data.Binary.Get             as G
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Char8       as C8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LC8
import qualified Data.Map.Strict             as M
import qualified Data.Text                   as LT
import qualified Data.Text.Lazy              as T
import qualified Data.Vector.Unboxed         as VU

type Name = String
type Code = (Char, Char)

getMagic :: AP.Parser BS.ByteString -> Get ()
getMagic magicParser = do
  a <- getLazyByteString magicLength
  case AP.parseOnly magicParser (LBS.toStrict a) of
    Right _    -> return ()
    Left error -> fail $ "wrong magic: \"" <> LC8.unpack a <> "\", expected: " <> error

getSegmentLength :: Get Int
getSegmentLength = fromIntegral <$> getInt64le

getSegmentPosition :: Get (Int, Int)
getSegmentPosition = (,)
  <$> (fromIntegral <$> getInt32le)
  <*> (fromIntegral <$> getInt32le)

getSegmentPositions :: Get [(Int, Int)]
getSegmentPositions = do
  n <- getSegmentLength
  replicateM n getSegmentPosition

getHeader :: AP.Parser BS.ByteString -> Get [(Int, Int)]
getHeader magicParser = do
  getMagic magicParser
  getSegmentPositions

mkDefaultSegment :: LBS.ByteString -> Segment LBS.ByteString
mkDefaultSegment bs = segment bs mempty

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _         = Nothing

extractFilenames :: LBS.ByteString -> [Text]
extractFilenames bs = either (const "") id . decodeUtf8' . LBS.toStrict <$> LBS.split 0 bs

newtype ByIndex a = ByIndex
  { unByIndex :: [a]
  } deriving (Eq, Show)

instance Monoid a => Monoid (ByIndex a) where
  mappend (ByIndex as) (ByIndex bs) = ByIndex (go as bs)
    where go (a:as) (b:bs) = (a <> b):go as bs
          go (a:as) bs     =  a      :go as bs
          go    as  (b:bs) =       b :go as bs
          go    []     []  = []
  mempty = ByIndex []

lookupSegment :: Text -> M.Map Text LBS.ByteString -> (LBS.ByteString -> [a]) -> [a]
lookupSegment filename directory f = case M.lookup filename directory of
  Just bs -> f bs
  Nothing -> []

extractTimes :: LBS.ByteString -> [POSIXTime]
extractTimes = ((^. from microseconds) <$>) <$> G.runGet go
  where go = do
          empty <- G.isEmpty
          if not empty
            then (:) <$> getInt64le <*> go
            else return []

extractSegments :: AP.Parser BS.ByteString -> LBS.ByteString -> Either String [Segment LBS.ByteString]
extractSegments magicParser bs = do
  bss <- extractSegmentByteStrings magicParser bs
  case bss of
    (as:ass) -> if ".asif/filenames\0" `LBS.isPrefixOf` as
      then do
        let filenames     = extractFilenames as
        let namedSegments = M.fromList (zip filenames bss)

        let metas = mempty
              <> ByIndex (replicate (length bss) mempty)
              <> ByIndex (metaFilename    <$> filenames)
              <> ByIndex (metaCreateTime  <$> lookupSegment ".asif/createtimes" namedSegments extractTimes)

        return $ uncurry segment <$> zip bss (unByIndex metas)
      else return (mkDefaultSegment <$> bss)
    _      -> return (mkDefaultSegment <$> bss)

extractNamedSegments :: AP.Parser BS.ByteString -> LBS.ByteString -> Either String (M.Map Text (Segment LBS.ByteString))
extractNamedSegments magicParser bs = do
  segments <- extractSegments magicParser bs
  let filenames = fromMaybe "" . (^. L.meta . L.filename) <$> segments
  return $ M.fromList $ zip filenames segments

extractSegmentByteStrings :: AP.Parser BS.ByteString -> LBS.ByteString -> Either String [LBS.ByteString]
extractSegmentByteStrings magicParser bs = case runGetOrFail (getHeader magicParser) bs of
  Left (_, _, err) -> Left err
  Right (_, _, header) -> do
    let segs = fmap (\(o, l) -> LBS.take (fromIntegral l) $ LBS.drop (fromIntegral o) bs) header
    forM_ (zip segs header) $ \(seg, (_, len)) ->
      when (LC8.length seg /= fromIntegral len) $
        fail "XXX segments not read correctly"
    return segs

segmentElements :: VU.Unbox a => Get a -> LBS.ByteString -> VU.Vector a
segmentElements f = VU.unfoldr step
  where step !s = case runGetOrFail f s of
          Left (_, _, err)    -> Nothing
          Right (!rem, _, !k) -> Just (k, rem)

segmentElements' :: Get a -> LBS.ByteString -> [a]
segmentElements' f = unfoldr step
  where step !s = case runGetOrFail f s of
          Left (_, _, err)    -> Nothing
          Right (!rem, _, !k) -> Just (k, rem)

segmentCidrs :: LBS.ByteString -> VU.Vector Word32
segmentCidrs = segmentElements getWord32le

getCode :: Get Code
getCode = do
  a <- getByteString 1
  b <- getByteString 1
  let [a'] = C8.unpack a
  let [b'] = C8.unpack b
  return (a', b')

getCodeMap :: LBS.ByteString -> LBS.ByteString -> M.Map Code String
getCodeMap kbs vbs = segmentMap kbs getCode vbs getNullString

getNameMap :: LBS.ByteString -> LBS.ByteString -> M.Map Word32 String
getNameMap kbs vbs = segmentMap kbs getWord32le vbs getNullString

getNullString :: Get String
getNullString = LC8.unpack <$> getLazyByteStringNul

segmentCodes :: LBS.ByteString -> VU.Vector Code
segmentCodes = segmentElements getCode

segmentIdentifiers :: LBS.ByteString -> VU.Vector Word32
segmentIdentifiers = segmentElements getWord32le

segmentMap :: (Ord a) => LBS.ByteString -> Get a -> LBS.ByteString -> Get b -> M.Map a b
segmentMap ks kf vs vf = foldr (\(k, v) m -> M.insert k v m) M.empty $ zip keys values
  where
    keys = segmentElements' kf ks
    values = segmentElements' vf vs

binarySearch :: (Ord a, VU.Unbox a) => a -> VU.Vector a -> Maybe Int
binarySearch key values = do
  guard (not (VU.null values))
  let idx = s 0 (VU.length values - 1)
  guard (idx > -1)
  return idx
  where
    s l h
      | l >= h =
        if (values VU.! h) <= key then h else -1
      | otherwise = do
        let m = l + (h - l) `div` 2
        if (values VU.! m) > key then s l m
        else do
          let result = s (m + 1) h
          if result == -1 then m
          else result

binarySearchExact :: (Ord a, VU.Unbox a) => a -> VU.Vector a -> Maybe Int
binarySearchExact key values = go values key 0 (VU.length values - 1)
  where
    go hay needle lo hi
      | hi < lo        = Nothing
      | pivot > needle = go hay needle lo (mid - 1)
      | pivot < needle = go hay needle (mid + 1) hi
      | otherwise      = Just mid
      where
        mid   = lo + (hi - lo) `div` 2
        pivot = hay VU.! mid
