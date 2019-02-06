module Arbor.File.Format.Asif.WriteSpec where

import Arbor.File.Format.Asif.Format
import Arbor.File.Format.Asif.Segment
import Arbor.File.Format.Asif.Write
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Data.Int
import Data.Semigroup                 ((<>))
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
    tplList <- forAll $ G.list (R.linear 0 50) gen

    let f1 = int64Segment                (\(a,_,_) -> a) "first"
    let f2 = word16Segment               (\(_,a,_) -> a) "second"
    let f3 = nullTerminatedStringSegment (\(_,_,a) -> T.toStrict a) "third"
    let fld = f1 <> f2 <> f3

    lbs <- buildAsifBytestring "wxyz" Nothing fld tplList

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs

    [names, times, types, l1,l2,l3] <- forAll $ pure (segmentValues <$> segments)

    ((\(a,_,_) -> SInt64 a) <$> tplList) === l1
    ((\(_,a,_) -> SWord16 a) <$> tplList) === l2
    ((\(_,_,a) -> SString . T.encodeUtf8 $ a) <$> tplList) === l3

-----

  it "should write out and read back in a word8 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.word8 R.linearBounded)

    lbs <- buildAsifBytestring "wxyz" Nothing (word8Segment id "word8") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SWord8 <$> lst) === seg

  it "should write out and read back in a word16 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.word16 R.linearBounded)

    lbs <- buildAsifBytestring "wxyz" Nothing (word16Segment id "word16") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SWord16 <$> lst) === seg

  it "should write out and read back in a word32 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.word32 R.linearBounded)

    lbs <- buildAsifBytestring "wxyz" Nothing (word32Segment id "word32") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SWord32 <$> lst) === seg

  it "should write out and read back in a word64 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.word64 R.linearBounded)

    lbs <- buildAsifBytestring "wxyz" Nothing (word64Segment id "word64") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SWord64 <$> lst) === seg

-----

  it "should write out and read back in a int8 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.int8 R.linearBounded)

    lbs <- buildAsifBytestring "wxyz" Nothing (int8Segment id "int8") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SInt8 <$> lst) === seg

  it "should write out and read back in a int16 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.int16 R.linearBounded)

    lbs <- buildAsifBytestring "wxyz" Nothing (int16Segment id "int16") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SInt16 <$> lst) === seg

  it "should write out and read back in a int32 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.int32 R.linearBounded)

    lbs <- buildAsifBytestring "wxyz" Nothing (int32Segment id "int32") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SInt32 <$> lst) === seg

  it "should write out and read back in a int64 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.int64 R.linearBounded)

    lbs <- buildAsifBytestring "wxyz" Nothing (int64Segment id "int64") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SInt64 <$> lst) === seg


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
