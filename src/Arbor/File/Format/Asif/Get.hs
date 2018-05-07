module Arbor.File.Format.Asif.Get where

import Arbor.File.Format.Asif.ByteString.Builder
import Control.Monad
import Data.Binary.Get
import Data.Monoid

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8

getMagic :: AP.Parser BS.ByteString -> Get ()
getMagic magicParser = do
  a <- getLazyByteString magicLength
  case AP.parseOnly magicParser (LBS.toStrict a) of
    Right _           -> return ()
    Left errorMessage -> fail $ "wrong magic: \"" <> LC8.unpack a <> "\", expected: " <> errorMessage

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

getCode :: Get (Char, Char)
getCode = do
  a <- getByteString 1
  b <- getByteString 1
  let [a'] = C8.unpack a
  let [b'] = C8.unpack b
  return (a', b')

getNullString :: Get String
getNullString = LC8.unpack <$> getLazyByteStringNul
