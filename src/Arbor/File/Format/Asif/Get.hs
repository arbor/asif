module Arbor.File.Format.Asif.Get where

import Arbor.File.Format.Asif.ByteString.Builder
import Arbor.File.Format.Asif.Data.Ip            (word32ToIpv4, word32x4ToIpv6)
import Control.Lens
import Control.Monad
import Data.Binary.Get
import Data.Monoid                               ((<>))
import Data.Text                                 (Text)
import Data.Thyme.Clock                          (microseconds)
import Data.Thyme.Clock.POSIX                    (POSIXTime)
import Data.Thyme.Time.Core                      (UTCTime, posixSecondsToUTCTime)
import Data.Word                                 (Word32)
import HaskellWorks.Data.Network.Ip.Validity     (Canonical)

import qualified Data.Attoparsec.ByteString        as AP
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.ByteString.Lazy.Char8        as LC8
import qualified Data.Text.Encoding                as T
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as IP4
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as IP6

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

getTimeMillis :: Get UTCTime
getTimeMillis =
  let toTime ms = (ms * 1000) ^. from microseconds & posixSecondsToUTCTime
  in getInt64le <&> toTime

getTimeMicros :: Get UTCTime
getTimeMicros =
  let toTime ms = ms ^. from microseconds & posixSecondsToUTCTime
  in getInt64le <&> toTime

getWord32x4 :: Get (Word32, Word32, Word32, Word32)
getWord32x4 = do
  a <- getWord32be
  b <- getWord32be
  c <- getWord32be
  d <- getWord32be
  return (a, b, c, d)

getBool :: Get Bool
getBool = getWord8 <&> (/= 0)

getIpv4 :: Get IP4.IpAddress
getIpv4 = getWord32le <&> word32ToIpv4

getIpv6 :: Get IP6.IpAddress
getIpv6 = getWord32x4 <&> word32x4ToIpv6

getIpv4Block :: Get (IP4.IpBlock Canonical)
getIpv4Block =
  let toIpBlock w32 w8 = IP4.IpBlock (IP4.IpAddress w32) (IP4.IpNetMask w8)
  in toIpBlock <$> getWord32le <*> getWord8

getIpv6Block :: Get (IP6.IpBlock Canonical)
getIpv6Block =
  let toIpBlock w128 w8 = IP6.IpBlock (IP6.IpAddress w128) (IP6.IpNetMask w8)
  in toIpBlock <$> getWord32x4 <*> getWord8

getTextUtf8Z :: Get Text
getTextUtf8Z = T.decodeUtf8 . LBS.toStrict <$> getLazyByteStringNul

getNullTerminatedString :: Get LBS.ByteString
getNullTerminatedString = getLazyByteStringNul

