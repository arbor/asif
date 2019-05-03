module Arbor.File.Format.Asif.Data.Ip
  ( stringToIpv4
  , stringToIpv6
  , ipv4ToString
  , ipv4CidrToString
  , isIpv4
  , ipv6ToString
  , ipv6toStringCollapseV4
  , ipv6CidrToString
  , ipv6CidrToStringCollapseV4
  , word32ToIpv4
  , word32x4ToIpv6
  , ipv4ToWord32
  , ipv6ToWord32x4
  , ipv4ToIpv6
  , word64ToIpList
  ) where

import Arbor.File.Format.Asif.Data.Read
import Control.Lens                     ((&))
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import Text.Read

import qualified Data.Bits                         as B
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as IP4
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as IP6

stringToIpv4 :: String -> Maybe IP4.IpAddress
stringToIpv4 str = if '.' `elem` str
  then readMaybe str
  else word32ToIpv4 <$> stringToAnyDigits str

stringToIpv6 :: String -> Maybe IP6.IpAddress
stringToIpv6 = readMaybe

ipv4ToString :: IP4.IpAddress -> String
ipv4ToString = IP4.showIpAddress

ipv4CidrToString :: IP4.IpBlock v -> String
ipv4CidrToString = show

isIpv4 :: IP6.IpAddress -> Maybe IP4.IpAddress
isIpv4 ip =
  let (a, b, c, d) = ipv6ToWord32x4 ip
  in if a == 0 && b == 0 && c == 0xFFFF
    then Just $ IP4.IpAddress d
    else Nothing

ipv6ToString :: IP6.IpAddress -> String
ipv6ToString = IP6.showIpAddress

ipv6toStringCollapseV4 :: IP6.IpAddress -> String
ipv6toStringCollapseV4 ip
  = case isIpv4 ip of
    Just d -> d & ipv4ToString
    _      -> ip & ipv6ToString

ipv6CidrToString :: IP6.IpBlock v -> String
ipv6CidrToString = show

ipv6CidrToStringCollapseV4 :: IP6.IpBlock v -> String
ipv6CidrToStringCollapseV4 block =
  let IP6.IpBlock ip (IP6.IpNetMask m) = block
  in case isIpv4 ip of
    Just d  -> ipv4CidrToString $ IP4.IpBlock d (IP4.IpNetMask m)
    Nothing -> ipv6CidrToString block

word32ToIpv4 :: Word32 -> IP4.IpAddress
word32ToIpv4 = IP4.IpAddress

word32x4ToIpv6 :: (Word32, Word32, Word32, Word32) -> IP6.IpAddress
word32x4ToIpv6 = IP6.IpAddress

ipv4ToWord32 :: IP4.IpAddress -> Word32
ipv4ToWord32 = IP4.word

ipv6ToWord32x4 :: IP6.IpAddress -> (Word32, Word32, Word32, Word32)
ipv6ToWord32x4 (IP6.IpAddress w128) = w128

ipv4ToIpv6 :: IP4.IpAddress -> IP6.IpAddress
ipv4ToIpv6 = IP6.fromIpv4

-- This takes a Word64 from a Bitmap, and converts it to a list of IPs.
-- The integer argument is an index into the bitmap
word64ToIpList :: Int -> Word64 -> [Word32] -> [Word32]
word64ToIpList _ 0 = id
word64ToIpList o w = (ip:) . word64ToIpList o (w .&. comp b)
  where p  = B.countTrailingZeros w
        hi = o .<. 6
        ip = fromIntegral (p .|. hi)
        b  = 1 .<. fromIntegral p
