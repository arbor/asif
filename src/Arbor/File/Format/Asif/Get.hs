module Arbor.File.Format.Asif.Get where

import Arbor.File.Format.Asif.ByteString.Builder
import Control.Lens
import Control.Monad
import Data.Binary.Get
import Data.Monoid
import Data.Text                                 (Text)
import Data.Thyme.Clock                          (microseconds)
import Data.Thyme.Clock.POSIX                    (POSIXTime)

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text.Encoding         as T

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

getTimeMicro64 :: Get POSIXTime
getTimeMicro64 = (^. from microseconds) <$> getInt64le

getTextUtf8Z :: Get Text
getTextUtf8Z = T.decodeUtf8 . LBS.toStrict <$> getLazyByteStringNul
