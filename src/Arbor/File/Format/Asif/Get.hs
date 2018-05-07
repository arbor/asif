module Arbor.File.Format.Asif.Get where

import Arbor.File.Format.Asif.ByteString.Builder
import Arbor.File.Format.Asif.Format             (Format)
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

getCode :: Get (Char, Char)
getCode = do
  a <- getByteString 1
  b <- getByteString 1
  let [a'] = C8.unpack a
  let [b'] = C8.unpack b
  return (a', b')

getNullString :: Get String
getNullString = LC8.unpack <$> getLazyByteStringNul
