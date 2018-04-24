{-# LANGUAGE ScopedTypeVariables #-}

module Arbor.File.Format.Asif.ByteString.BuilderSpec
  ( spec
  ) where

import Arbor.File.Format.Asif.ByteString.Builder
import Conduit
import Control.Lens
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import System.IO                                 (openTempFile)
import Test.Hspec

import qualified Arbor.File.Format.Asif                    as C
import qualified Arbor.File.Format.Asif.ByteString.Builder as LB
import qualified Arbor.File.Format.Asif.Lens               as L
import qualified Data.Attoparsec.ByteString                as AP
import qualified Data.ByteString                           as BS
import qualified Data.ByteString.Lazy                      as LBS
import qualified Hedgehog.Gen                              as G
import qualified Hedgehog.Range                            as R
import qualified System.IO                                 as IO

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

    let segs = LB.segmentsRawC "wxyz"
          [ h1
          , h2
          , h3
          ]

    liftIO $ runConduit $ segs .| sinkHandle he

    liftIO $ IO.hSeek he IO.AbsoluteSeek 0

    contents <- liftIO $ LBS.hGetContents he

    _ <- forAll $ pure $ LBS.unpack contents

    let Right segments = C.extractSegments (AP.string "seg:wxyz") contents

    ((^. L.payload) <$> segments) === [b1, b2, b3]
