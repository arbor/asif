{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Arbor.File.Format.Asif.Extract
  ( cidrs
  , codeMap
  , codes
  , filenames
  , formats
  , identifiers
  , list
  , nameMap
  , times
  , vectorBoxed
  , vectorUnboxed
  ) where

import Arbor.File.Format.Asif.Format   (Format)
import Arbor.File.Format.Asif.Get
import Arbor.File.Format.Asif.Whatever
import Control.Lens
import Data.Binary.Get
import Data.List                       hiding (map)
import Data.Text                       (Text)
import Data.Text.Encoding              (decodeUtf8')
import Data.Text.Encoding.Error
import Data.Thyme.Clock                (microseconds)
import Data.Thyme.Clock.POSIX          (POSIXTime)
import Data.Word
import Prelude                         hiding (map)

import qualified Data.Binary.Get      as G
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as M
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as VU

vectorBoxed :: Get a -> LBS.ByteString -> V.Vector a
vectorBoxed g = V.unfoldr step
  where step !s = case runGetOrFail g s of
          Left (_, _, _)     -> Nothing
          Right (!rs, _, !k) -> Just (k, rs)

vectorUnboxed :: VU.Unbox a => Get a -> LBS.ByteString -> VU.Vector a
vectorUnboxed g = VU.unfoldr step
  where step !s = case runGetOrFail g s of
          Left (_, _, _)     -> Nothing
          Right (!rs, _, !k) -> Just (k, rs)

list :: Get a -> LBS.ByteString -> [a]
list g = G.runGet go
  where go = do
          empty <- G.isEmpty
          if not empty
            then (:) <$> g <*> go
            else return []

filenames :: LBS.ByteString -> [Text]
filenames bs = either (const "") id . decodeUtf8' . LBS.toStrict <$> LBS.split 0 bs

times :: LBS.ByteString -> [POSIXTime]
times = ((^. from microseconds) <$>) <$> G.runGet go
  where go = do
          empty <- G.isEmpty
          if not empty
            then (:) <$> getInt64le <*> go
            else return []

formats :: LBS.ByteString -> [Maybe (Whatever Format)]
formats bs = LBS.split 0 bs <&> decodeUtf8' . LBS.toStrict <&> convert
  where convert :: Either UnicodeException Text -> Maybe (Whatever Format)
        convert (Left _)   = Nothing
        convert (Right "") = Nothing
        convert (Right t)  = Just (tReadWhatever t)

cidrs :: LBS.ByteString -> VU.Vector Word32
cidrs = vectorUnboxed getWord32le

codeMap :: LBS.ByteString -> LBS.ByteString -> M.Map (Char, Char) String
codeMap kbs vbs = map kbs getCode vbs getNullString

nameMap :: LBS.ByteString -> LBS.ByteString -> M.Map Word32 String
nameMap kbs vbs = map kbs getWord32le vbs getNullString

codes :: LBS.ByteString -> VU.Vector (Char, Char)
codes = vectorUnboxed getCode

identifiers :: LBS.ByteString -> VU.Vector Word32
identifiers = vectorUnboxed getWord32le

map :: (Ord a) => LBS.ByteString -> Get a -> LBS.ByteString -> Get b -> M.Map a b
map ks kf vs vf = foldr (\(k, v) m -> M.insert k v m) M.empty $ zip keys values
  where
    keys = list kf ks
    values = list vf vs
