module Arbor.File.Format.Asif.Data.Ip
  ( stringToIpv4
  , ipv4ToString
  , word32ToIpv4
  , ipv4ToWord32
  ) where

import Arbor.File.Format.Asif.Data.Read
import Control.Lens                     ((^.))
import Control.Lens.Iso                 (from)
import Control.Monad
import Data.IP
import Data.Thyme.Clock                 (microseconds)
import Data.Thyme.Clock.POSIX           (POSIXTime)
import Data.Word
import System.Endian
import Text.Read

stringToIpv4 :: String -> Maybe IPv4
stringToIpv4 str = if '.' `elem` str
  then readMaybe str
  else word32ToIpv4 <$> stringToAnyDigits str

ipv4ToString :: IPv4 -> String
ipv4ToString = show

word32ToIpv4 :: Word32 -> IPv4
word32ToIpv4 = fromHostAddress . toBE32

ipv4ToWord32 :: IPv4 -> Word32
ipv4ToWord32 = fromBE32 . toHostAddress
