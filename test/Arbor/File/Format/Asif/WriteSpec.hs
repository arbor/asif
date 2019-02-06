module Arbor.File.Format.Asif.WriteSpec where

import Arbor.File.Format.Asif.Format
import Arbor.File.Format.Asif.Segment
import Arbor.File.Format.Asif.Write
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Data.Int
import Data.Word

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Encoding    as T
import qualified Hedgehog.Gen               as G
import qualified Hedgehog.Range             as R


{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "Arbor.File.Format.Asif.Write" $ do
  it "should write out some stuff, then read it back in" $ require $ property $ do
    tplList <- forAll $ G.list (R.linear 1 50) gen

    let f1 = int64Segment                (\(a,_,_) -> a) "first"
    let f2 = word16Segment               (\(_,a,_) -> a) "second"
    let f3 = nullTerminatedStringSegment (\(_,_,a) -> T.toStrict a) "third"
    let fld = f1 <> f2 <> f3

    lbs <- liftResourceT $ buildAsifBytestring "wxyz" Nothing fld tplList

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs

    [names, times, types, l1,l2,l3] <- forAll $ pure (segmentValues <$> segments)

    ((\(a,_,_) -> SInt64 a) <$> tplList) === l1
    ((\(_,a,_) -> SWord16 a) <$> tplList) === l2
    ((\(_,_,a) -> SString . T.encodeUtf8 $ a) <$> tplList) === l3

gen :: MonadGen m => m (Int64, Word16, T.Text)
gen
  = (,,)
  <$> G.int64 R.linearBounded
  <*> G.word16 R.linearBounded
  <*> (T.fromStrict <$> G.text (R.linear 0 32) (G.filter (/= toEnum 0) G.unicode))

instance MonadResource m => MonadResource (PropertyT m) where
  liftResourceT = lift . liftResourceT

-- I have no idea how this doesn't exist, but whatever. ¯\_(ツ)_/¯
instance MonadResource IO where
  liftResourceT = runResourceT
