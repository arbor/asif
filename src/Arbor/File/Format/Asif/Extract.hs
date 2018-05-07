{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Arbor.File.Format.Asif.Extract
  ( extractFilenames
  , extractTimes
  , extractFormats
  , getCodeMap
  , getNameMap
  , segmentCidrs
  , segmentCodes
  , segmentElements
  , segmentIdentifiers
  ) where

import Arbor.File.Format.Asif.Format   (Format)
import Arbor.File.Format.Asif.Get
import Arbor.File.Format.Asif.Whatever
import Control.Lens
import Data.Binary.Get
import Data.List
import Data.Text                       (Text)
import Data.Text.Encoding              (decodeUtf8')
import Data.Text.Encoding.Error
import Data.Thyme.Clock                (microseconds)
import Data.Thyme.Clock.POSIX          (POSIXTime)
import Data.Word

import qualified Data.Binary.Get      as G
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as M
import qualified Data.Vector.Unboxed  as VU

extractFilenames :: LBS.ByteString -> [Text]
extractFilenames bs = either (const "") id . decodeUtf8' . LBS.toStrict <$> LBS.split 0 bs

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
        convert (Left _)   = Nothing
        convert (Right "") = Nothing
        convert (Right t)  = Just (tReadWhatever t)


segmentElements :: VU.Unbox a => Get a -> LBS.ByteString -> VU.Vector a
segmentElements f = VU.unfoldr step
  where step !s = case runGetOrFail f s of
          Left (_, _, _)     -> Nothing
          Right (!rs, _, !k) -> Just (k, rs)

segmentElements' :: Get a -> LBS.ByteString -> [a]
segmentElements' f = unfoldr step
  where step !s = case runGetOrFail f s of
          Left (_, _, _)     -> Nothing
          Right (!rs, _, !k) -> Just (k, rs)

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
