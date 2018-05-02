{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Arbor.File.Format.AsifSpec
( spec
) where


import Data.Semigroup                       ((<>))
import Data.Word                            (Word32)


import HaskellWorks.Hspec.Hedgehog
import Hedgehog                             (forAll, property, (===))
import Test.Hspec

import Debug.Trace
import System.IO.Unsafe

import Arbor.File.Format.Asif

import qualified Data.Map.Strict             as M
import qualified Data.Vector.Unboxed         as VU

spec :: Spec
spec = describe "Arbor.File.Format.AsifSpec" $ do
  it "binarySearch: Find nothing in empty array" $ require $ property $ do
    let v = VU.fromList ([] :: [Word32])
    binarySearch 0 v === Nothing

  it "binarySearch: Find nothing if less than all" $ require $ property $ do
    let v = VU.fromList ([1] :: [Word32])
    binarySearch 0 v === Nothing

  it "binarySearch: Find if equal" $ require $ property $ do
    let v = VU.fromList ([1] :: [Word32])
    binarySearch 1 v === Just 0

  it "binarySearch: Find if more than some" $ require $ property $ do
    let v = VU.fromList ([1] :: [Word32])
    binarySearch 2 v === Just 0

  it "binarySearch: Find if in interval" $ require $ property $ do
    let v = VU.fromList ([1,3] :: [Word32])
    binarySearch 2 v === Just 0

  it "binarySearchExact: Find nothing in empty array" $ require $ property $ do
    let v = VU.fromList ([] :: [Word32])
    binarySearchExact 0 v === Nothing

  it "binarySearchExact: Nothing if less" $ require $ property $ do
    let v = VU.fromList ([1] :: [Word32])
    binarySearchExact 0 v === Nothing

  it "binarySearchExact: Find if equal" $ require $ property $ do
    let v = VU.fromList ([1] :: [Word32])
    binarySearchExact 1 v === Just 0

  it "binarySearchExact: Nothing if more" $ require $ property $ do
    let v = VU.fromList ([1] :: [Word32])
    binarySearchExact 2 v === Nothing

  it "binarySearchExact: Nothing if in interval" $ require $ property $ do
    let v = VU.fromList ([1,3] :: [Word32])
    binarySearchExact 2 v === Nothing
