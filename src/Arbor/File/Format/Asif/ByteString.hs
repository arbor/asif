
module Arbor.File.Format.Asif.ByteString where

import Data.ByteString (ByteString)

import qualified Data.ByteString as BS

chunkBy :: Int -> ByteString -> [ByteString]
chunkBy n bs = case (BS.take n bs, BS.drop n bs) of
  (as, zs) -> if BS.null zs
    then [as]
    else as:chunkBy n zs
