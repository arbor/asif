module Arbor.File.Format.Asif.Data.Ip
  ( stringToIpv4
  , stringToIpv6
  , ipv4ToString
  , ipv6ToString
  , word32ToIpv4
  , word32x4ToIpv6
  , ipv4ToWord32
  , ipv6ToWord32x4
  , ipv4ToIpv6
  ) where

import Arbor.File.Format.Asif.Data.Read
import Data.IP
import Data.Word
import System.Endian
import Text.Read

stringToIpv4 :: String -> Maybe IPv4
stringToIpv4 str = if '.' `elem` str
  then readMaybe str
  else word32ToIpv4 <$> stringToAnyDigits str

stringToIpv6 :: String -> Maybe IPv6
stringToIpv6 = readMaybe

ipv4ToString :: IPv4 -> String
ipv4ToString = show

ipv6ToString :: IPv6 -> String
ipv6ToString = show

word32ToIpv4 :: Word32 -> IPv4
word32ToIpv4 = fromHostAddress . toBE32

word32x4ToIpv6 :: (Word32, Word32, Word32, Word32) -> IPv6
word32x4ToIpv6 = fromHostAddress6

ipv4ToWord32 :: IPv4 -> Word32
ipv4ToWord32 = fromBE32 . toHostAddress

ipv6ToWord32x4 :: IPv6 -> (Word32, Word32, Word32, Word32)
ipv6ToWord32x4 = toHostAddress6

ipv4ToIpv6 :: IPv4 -> IPv6
ipv4ToIpv6 = ipv4ToIPv6
