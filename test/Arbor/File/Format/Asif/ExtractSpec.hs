{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arbor.File.Format.Asif.ExtractSpec
  ( spec
  ) where

import Arbor.File.Format.Asif.Extract
import Control.Lens

import qualified Data.Binary.Get         as G
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LBS

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.List      as L
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "Arbor.File.Format.Asif.ExtractSpec" $ do
  it "listLazy should extract correct values" $ require $ property $ do
    ts <- forAll $ Gen.list (Range.linear 0 20) (Gen.int64 (Range.linear 0 maxBound))
    let body = ts <&> BB.int64LE & mconcat & BB.toLazyByteString
    listLazy G.getInt64le body === fmap Right ts
