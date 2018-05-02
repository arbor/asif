
module Arbor.File.Format.Asif.ByteString.Lazy where

import Data.ByteString.Lazy (ByteString)
import Data.Int

import qualified Data.ByteString.Lazy as LBS

chunkBy :: Int64 -> ByteString -> [ByteString]
chunkBy n bs = case (LBS.take n bs, LBS.drop n bs) of
  (as, zs) -> if LBS.null zs
    then [as]
    else as:chunkBy n zs
