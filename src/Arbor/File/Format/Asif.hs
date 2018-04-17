{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Arbor.File.Format.Asif
  ( binarySearch
  , extractSegments
  , getCodeMap
  , getNameMap
  , segmentCidrs
  , segmentCodes
  , segmentIdentifiers
  ) where

import Arbor.File.Format.Asif.ByteString.Builder
import Control.Lens
import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Builder
import Data.Either.Combinators
import Data.Int
import Data.List
import Data.Monoid
import Data.Traversable
import Data.Word

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Map.Strict            as M
import qualified Data.Vector.Unboxed        as VU

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

extractSegments :: AP.Parser BS.ByteString -> LBS.ByteString -> Either String [LBS.ByteString]
extractSegments magicParser bs = case runGetOrFail (getHeader magicParser) bs of
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
  let idx = s key values 0 (VU.length values - 1)
  if idx > -1 then Just idx
  else Nothing
  where
    s :: (Ord a, VU.Unbox a) => a -> VU.Vector a -> Int -> Int -> Int
    s k v l h
      | l >= h =
        if (v VU.! h) <= k then h else -1
      | otherwise = do
        let m = l + (h - l) `div` 2
        if (v VU.! m) > k then s k v l m
        else do
          let result = s k v (m + 1) h
          if result == -1 then m
          else result
