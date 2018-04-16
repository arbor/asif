{-# LANGUAGE ScopedTypeVariables #-}

module App.ByteString.Lazy.BuilderSpec
( spec
)
where

import App.ByteString.Lazy.Builder
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LBS
import qualified SegmentedFormat         as C
import qualified Hedgehog.Gen            as G
import qualified Hedgehog.Range          as R

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "App.ByteString.Lazy.Builder" $ do
  it "should extract segments" $ require $ withTests 1 $ property $ do
    b1 :: LBS.ByteString <- forAll $ flip LBS.replicate 65 <$> G.int64 (R.linear 0 10)
    b2 :: LBS.ByteString <- forAll $ flip LBS.replicate 66 <$> G.int64 (R.linear 0 10)
    b3 :: LBS.ByteString <- forAll $ flip LBS.replicate 67 <$> G.int64 (R.linear 0 10)

    let segs = B.toLazyByteString (segments 0 'c' [withSize b1, withSize b2, withSize b3])
    C.extractSegments (magicString 'c') segs === Right [b1, b2, b3]
