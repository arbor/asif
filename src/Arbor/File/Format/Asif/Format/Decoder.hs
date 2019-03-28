module Arbor.File.Format.Asif.Format.Decoder where

import Arbor.File.Format.Asif.Data.Ip
import Control.Lens
import Data.Thyme.Time.Core
import Data.Word                             (Word32)
import HaskellWorks.Data.Network.Ip.Validity (Canonical)

import qualified Data.Binary.Get                   as G
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as IP4
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as IP6

getWord32x4 :: G.Get (Word32, Word32, Word32, Word32)
getWord32x4 = do
  a <- G.getWord32be
  b <- G.getWord32be
  c <- G.getWord32be
  d <- G.getWord32be
  return (a, b, c, d)

getBool :: G.Get Bool
getBool = G.getWord8 <&> (/= 0)

getIpv4 :: G.Get IP4.IpAddress
getIpv4 = G.getWord32le <&> word32ToIpv4

getIpv6 :: G.Get IP6.IpAddress
getIpv6 = getWord32x4 <&> word32x4ToIpv6

getIpv4Block :: G.Get (IP4.IpBlock Canonical)
getIpv4Block =
  let toIpBlock w32 w8 = IP4.IpBlock (IP4.IpAddress w32) (IP4.IpNetMask w8)
  in toIpBlock <$> G.getWord32le <*> G.getWord8

getIpv6Block :: G.Get (IP6.IpBlock Canonical)
getIpv6Block =
  let toIpBlock w128 w8 = IP6.IpBlock (IP6.IpAddress w128) (IP6.IpNetMask w8)
  in toIpBlock <$> getWord32x4 <*> G.getWord8

getTimeMillis :: G.Get UTCTime
getTimeMillis =
  let toTime ms = (ms * 1000) ^. from microseconds & posixSecondsToUTCTime
  in G.getInt64le <&> toTime

getTimeMicros :: G.Get UTCTime
getTimeMicros =
  let toTime ms = ms ^. from microseconds & posixSecondsToUTCTime
  in G.getInt64le <&> toTime
