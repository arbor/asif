{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Arbor.File.Format.Asif.Extract
  ( formats
  , list
  , listLazy
  , map
  , vectorBoxed
  , vectorUnboxed
  , bitmap
  ) where

import Arbor.File.Format.Asif.Data.Ip
import Arbor.File.Format.Asif.Format.Type (Format)
import Arbor.File.Format.Asif.Whatever
import Control.Lens
import Data.Binary.Get
import Data.List                          hiding (map)
import Data.Text                          (Text)
import Data.Text.Encoding                 (decodeUtf8')
import Data.Text.Encoding.Error
import Prelude                            hiding (map)

import qualified Arbor.File.Format.Asif.ByteString.Lazy as LBS
import qualified Data.Binary.Get                        as G
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.List                              as L
import qualified Data.Map.Strict                        as M
import qualified Data.Vector                            as V
import qualified Data.Vector.Unboxed                    as VU
import qualified HaskellWorks.Data.Network.Ip.Ipv4      as IP4

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

listLazy :: Get a -> LBS.ByteString -> [Either String a]
listLazy g bs =
  flip L.unfoldr bs $ \acc ->
    if LBS.null acc then Nothing
    else case runGetOrFail g acc of
      Left (_, _, err)  -> Just (Left err, LBS.empty)
      Right (bs', _, a) -> Just (Right a, bs')

map :: (Ord a) => LBS.ByteString -> Get a -> LBS.ByteString -> Get b -> M.Map a b
map ks kf vs vf = foldr (\(k, v) m -> M.insert k v m) M.empty $ zip keys values
  where
    keys = list kf ks
    values = list vf vs

formats :: LBS.ByteString -> [Maybe (Whatever Format)]
formats bs = LBS.split 0 bs <&> decodeUtf8' . LBS.toStrict <&> convert
  where convert :: Either UnicodeException Text -> Maybe (Whatever Format)
        convert (Left _)   = Nothing
        convert (Right "") = Nothing
        convert (Right t)  = Just (tReadWhatever t)

bitmap :: LBS.ByteString -> [IP4.IpAddress]
bitmap lbs =
  zip [0..] (G.runGet G.getWord64le <$> LBS.chunkBy 8 lbs) >>= \(idx, w64) ->
    word64ToIpList idx w64 [] <&> word32ToIpv4
