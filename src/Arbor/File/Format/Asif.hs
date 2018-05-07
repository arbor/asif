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

import Arbor.File.Format.Asif.ByIndex
import Arbor.File.Format.Asif.ByteString.Builder
import Arbor.File.Format.Asif.Format             (Format)
import Arbor.File.Format.Asif.Get
import Arbor.File.Format.Asif.Search
import Arbor.File.Format.Asif.Text
import Arbor.File.Format.Asif.Type
import Arbor.File.Format.Asif.Whatever
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
import Data.Text.Encoding.Error
import Data.Thyme.Clock                          (microseconds)
import Data.Thyme.Clock.POSIX                    (POSIXTime)
import Data.Traversable
import Data.Word

import qualified Arbor.File.Format.Asif.Format as F
import qualified Arbor.File.Format.Asif.Lens   as L
import qualified Data.Attoparsec.ByteString    as AP
import qualified Data.Binary.Get               as G
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LC8
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as LT
import qualified Data.Text.Lazy                as T
import qualified Data.Vector.Unboxed           as VU

mkDefaultSegment :: LBS.ByteString -> Segment LBS.ByteString
mkDefaultSegment bs = segment bs mempty

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _         = Nothing

extractFilenames :: LBS.ByteString -> [Text]
extractFilenames bs = either (const "") id . decodeUtf8' . LBS.toStrict <$> LBS.split 0 bs

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

extractFormats :: LBS.ByteString -> [Maybe (Whatever Format)]
extractFormats bs = LBS.split 0 bs <&> decodeUtf8' . LBS.toStrict <&> convert
  where convert :: Either UnicodeException Text -> Maybe (Whatever Format)
        convert (Left e)   = Nothing
        convert (Right "") = Nothing
        convert (Right t)  = Just (tReadWhatever t)

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
              <> ByIndex (metaMaybeFormat <$> lookupSegment ".asif/formats"     namedSegments extractFormats)

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

getCodeMap :: LBS.ByteString -> LBS.ByteString -> M.Map (Char, Char) String
getCodeMap kbs vbs = segmentMap kbs getCode vbs getNullString

getNameMap :: LBS.ByteString -> LBS.ByteString -> M.Map Word32 String
getNameMap kbs vbs = segmentMap kbs getWord32le vbs getNullString

segmentCodes :: LBS.ByteString -> VU.Vector (Char, Char)
segmentCodes = segmentElements getCode

segmentIdentifiers :: LBS.ByteString -> VU.Vector Word32
segmentIdentifiers = segmentElements getWord32le

segmentMap :: (Ord a) => LBS.ByteString -> Get a -> LBS.ByteString -> Get b -> M.Map a b
segmentMap ks kf vs vf = foldr (\(k, v) m -> M.insert k v m) M.empty $ zip keys values
  where
    keys = segmentElements' kf ks
    values = segmentElements' vf vs
