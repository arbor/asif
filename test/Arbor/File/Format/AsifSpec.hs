{-# LANGUAGE FlexibleContexts #-}

module Arbor.File.Format.AsifSpec
( spec
) where

import Arbor.File.Format.Asif
import Data.Semigroup              ((<>))
import Data.Word                   (Word32)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog                    (forAll, property, (===))
import System.IO.Unsafe
import Test.Hspec

import qualified Data.Map.Strict     as M
import qualified Data.Vector.Unboxed as VU

spec :: Spec
spec = describe "Arbor.File.Format.AsifSpec" $ do
  it "binarySearch: Find nothing in empty array" $ requireTest $ do
    let v = VU.fromList ([] :: [Word32])
    binarySearch 0 v === Nothing

  it "binarySearch: Find nothing if less than all" $ requireTest $ do
    let v = VU.fromList ([1] :: [Word32])
    binarySearch 0 v === Nothing

  it "binarySearch: Find if equal" $ requireTest $ do
    let v = VU.fromList ([1] :: [Word32])
    binarySearch 1 v === Just 0

  it "binarySearch: Find if more than some" $ requireTest $ do
    let v = VU.fromList ([1] :: [Word32])
    binarySearch 2 v === Just 0

  it "binarySearch: Find if in interval" $ requireTest $ do
    let v = VU.fromList ([1, 3] :: [Word32])
    binarySearch 2 v === Just 0

  it "binarySearchExact: Find nothing in empty array" $ requireTest $ do
    let v = VU.fromList ([] :: [Word32])
    binarySearchExact 0 v === Nothing

  it "binarySearchExact: Nothing if less" $ requireTest $ do
    let v = VU.fromList ([1] :: [Word32])
    binarySearchExact 0 v === Nothing

  it "binarySearchExact: Find if equal" $ requireTest $ do
    let v = VU.fromList ([1] :: [Word32])
    binarySearchExact 1 v === Just 0

  it "binarySearchExact: Nothing if more" $ requireTest $ do
    let v = VU.fromList ([1] :: [Word32])
    binarySearchExact 2 v === Nothing

  it "binarySearchExact: Nothing if in interval" $ requireTest $ do
    let v = VU.fromList ([1, 3] :: [Word32])
    binarySearchExact 2 v === Nothing
