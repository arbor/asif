{-# LANGUAGE ScopedTypeVariables #-}

module Arbor.File.Format.Asif.Data.IpSpec
  ( spec
  ) where

import Arbor.File.Format.Asif.Data.Ip
import Conduit
import Control.Lens
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import System.IO                      (openTempFile)
import Test.Hspec

import qualified Arbor.File.Format.Asif                    as C
import qualified Arbor.File.Format.Asif.ByteString.Builder as LB
import qualified Data.Attoparsec.ByteString                as AP
import qualified Data.ByteString                           as BS
import qualified Data.ByteString.Lazy                      as LBS
import qualified Hedgehog.Gen                              as G
import qualified Hedgehog.Range                            as R
import qualified System.IO                                 as IO

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "Arbor.File.Format.Asif.Data.Ip" $ do
  it "word32ToIpv4" $ requireTest $ do
    show (word32ToIpv4 1) === "0.0.0.1"
  it "ipv4ToString" $ requireTest $ do
    ipv4ToString (word32ToIpv4 1) === "0.0.0.1"
  it "stringToIpv4" $ requireTest $ do
    show (stringToIpv4 "0.0.0.1") === "Just 0.0.0.1"
  it "stringToIpv4" $ requireTest $ do
    ipv4ToWord32 (word32ToIpv4 1) === 1
