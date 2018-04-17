{-# LANGUAGE ScopedTypeVariables #-}

module App.ByteString.Lazy.BuilderSpec
  ( spec
  ) where

import App.ByteString.Lazy.Builder
import Conduit
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import System.IO                   (openTempFile)
import Test.Hspec

import qualified App.ByteString.Lazy.Builder as LB
import qualified Data.Attoparsec.ByteString  as AP
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Hedgehog.Gen                as G
import qualified Hedgehog.Range              as R
import qualified SegmentedFormat             as C
import qualified System.IO                   as IO

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "App.ByteString.Lazy.Builder" $ do
  it "should extract segments" $ require $ withTests 1 $ property $ do
    b1 :: LBS.ByteString <- forAll $ flip LBS.replicate 65 <$> G.int64 (R.linear 0 10)
    b2 :: LBS.ByteString <- forAll $ flip LBS.replicate 66 <$> G.int64 (R.linear 0 10)
    b3 :: LBS.ByteString <- forAll $ flip LBS.replicate 67 <$> G.int64 (R.linear 0 10)

    (_, h1) <- liftIO $ openTempFile "/tmp" "h1.tmp"
    (_, h2) <- liftIO $ openTempFile "/tmp" "h2.tmp"
    (_, h3) <- liftIO $ openTempFile "/tmp" "h3.tmp"

    (_, he) <- liftIO $ openTempFile "/tmp" "he.tmp"

    liftIO $ BS.hPut h1 (LBS.toStrict b1)
    liftIO $ BS.hPut h2 (LBS.toStrict b2)
    liftIO $ BS.hPut h3 (LBS.toStrict b3)

    let segs = LB.segmentsC 0 'x'
          [ h1
          , h2
          , h3
          ]

    liftIO $ runConduit $ segs .| sinkHandle he

    liftIO $ IO.hSeek he IO.AbsoluteSeek 0

    actual <- liftIO $ LBS.hGetContents he

    C.extractSegments (AP.string "seg:ganx") actual === Right [b1, b2, b3]
