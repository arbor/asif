{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arbor.File.Format.Asif.Format.SegmentValueSpec
  ( spec
  ) where

import           Arbor.File.Format.Asif.Data.Ip
import           Arbor.File.Format.Asif.Format   as F
import           Arbor.File.Format.Asif.Type
import           Arbor.File.Format.Asif.Whatever
import           Control.Lens
import qualified Data.List                       as L
import           Data.Semigroup                  ((<>))
import           Data.Thyme.Time.Core

import qualified Data.ByteString.Builder         as BB
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Lazy.Char8      as LC8

import           Arbor.TestUtils
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec

import qualified Hedgehog.Gen                    as Gen
import qualified Hedgehog.Range                  as Range

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "Arbor.File.Format.Asif.Format.RawValueSpec" $ do
  it "should parse strings" $ require $ property $ do
    bs <- forAll $ Gen.list (Range.linear 0 20) (genLazyBS (Range.linear 0 512) Gen.alphaNum)
    let body = mconcat $ (<> "\0") <$> bs
    let seg = segment body $ metaFormat (Known F.StringZ)
    segmentValues seg === fmap SString bs

  it "should parse chars" $ require $ property $ do
    bs <- forAll $ genLazyBS (Range.linear 0 512) Gen.latin1
    let expected = LC8.unpack bs <&> SChar
    let seg = segment bs $ metaFormat (Known F.Char)
    segmentValues seg === expected

  it "should parse time (micros)" $ require $ property $ do
    ts <- forAll $ Gen.list (Range.linear 0 20) (Gen.int64 (Range.linear 0 maxBound))
    let body = ts <&> BB.int64LE & mconcat & BB.toLazyByteString
    let times = ts ^.. each . from microseconds . to posixSecondsToUTCTime
    let seg = segment body $ metaFormat (Known F.TimeMicros64LE)
    segmentValues seg === fmap STime times

  it "should parse time (millis)" $ require $ property $ do
    ts <- forAll $ Gen.list (Range.linear 0 20) (Gen.int64 (Range.linear 0 maxBound) <&> (`div` 1000))
    let body = ts <&> BB.int64LE & mconcat & BB.toLazyByteString
    let times = ts ^.. each . to (*1000) . from microseconds . to posixSecondsToUTCTime
    let seg = segment body $ metaFormat (Known F.TimeMillis64LE)
    segmentValues seg === fmap STime times

  it "should parse int64" $ require $ property $ do
    testMany F.Int64LE SInt64 BB.int64LE (Gen.int64 (Range.linear 0 maxBound))

  it "should parse int32" $ require $ property $ do
    testMany F.Int32LE SInt32 BB.int32LE (Gen.int32 (Range.linear 0 maxBound))

  it "should parse int16" $ require $ property $ do
    testMany F.Int16LE SInt16 BB.int16LE (Gen.int16 (Range.linear 0 maxBound))

  it "should parse int8" $ require $ property $ do
    testMany F.Int8 SInt8 BB.int8 (Gen.int8 (Range.linear 0 maxBound))

  it "should parse word64" $ require $ property $ do
    testMany F.Word64LE SWord64 BB.word64LE (Gen.word64 (Range.linear 0 maxBound))

  it "should parse word32" $ require $ property $ do
    testMany F.Word32LE SWord32 BB.word32LE (Gen.word32 (Range.linear 0 maxBound))

  it "should parse word16" $ require $ property $ do
    testMany F.Word16LE SWord16 BB.word16LE (Gen.word16 (Range.linear 0 maxBound))

  it "should parse word8" $ require $ property $ do
    testMany F.Word8 SWord8 BB.word8 (Gen.word8 (Range.linear 0 maxBound))

  it "should parse BitString" $ require $ property $
    let gen = Gen.bytes (Range.linear 0 256) <&> LBS.fromStrict
    in testOne F.BitString SBitString BB.lazyByteString gen

  it "should parse Binary" $ require $ property $
    let gen = Gen.bytes (Range.linear 0 256) <&> LBS.fromStrict
    in testOne F.Binary SBinary BB.lazyByteString gen

  it "should parse Bitmap" $ require $ property $
    let gen = Gen.bytes (Range.linear 0 256) <&> LBS.fromStrict
    in testOne F.Bitmap SBitmap BB.lazyByteString gen

  it "should parse repeated BitString" $ require $ property $ do
    n <- forAll $ Gen.int (Range.linear 1 32)
    vs <- forAll $ Gen.list (Range.linear 0 20) (Gen.bytes (Range.singleton n) <&> LBS.fromStrict)
    let seg = segment (mconcat vs) $ metaFormat (Known (Repeat (fromIntegral n) F.BitString))
    segmentValues seg === fmap SBitString vs

  it  "should parse repeated chars" $ require $ property $ do
    n <- forAll $ Gen.int (Range.linear 1 32)
    vs <- forAll $ Gen.list (Range.linear 0 20) (Gen.string (Range.singleton n) Gen.alphaNum)
    let seg = segment (LC8.pack $ mconcat vs) $ metaFormat (Known (Repeat (fromIntegral n) F.Char))
    segmentValues seg === fmap (SList . fmap SChar) vs

  it "should parse repeated ips" $ require $ property $ do
    n <- forAll $ Gen.int (Range.linear 1 32)
    k <- forAll $ Gen.int (Range.linear 0 20)
    vs <- forAll $ Gen.list (Range.singleton (n*k)) (Gen.word32 Range.linearBounded)
    let body = vs <&> BB.word32LE & mconcat & BB.toLazyByteString
    let seg = segment body $ metaFormat (Known (Repeat (fromIntegral n) F.Ipv4))
    let expected = vs <&> (SIpv4 . word32ToIpv4) & chunksOf n <&> SList
    segmentValues seg === expected

genLazyBS :: MonadGen m => Range Int -> m Char -> m LBS.ByteString
genLazyBS rng gen = Gen.utf8 rng gen <&> LBS.fromStrict

testOne :: (Monad m, Show a)
        => Format
        -> (a -> SegmentValue)
        -> (a -> BB.Builder)
        -> Gen a
        -> PropertyT m ()
testOne fmt wrap put gen = do
  v <- forAll gen
  let body = put v & BB.toLazyByteString
  let seg = segment body $ metaFormat (Known fmt)
  segmentValues seg === [wrap v]

testMany :: (Monad m, Show a)
          => Format
          -> (a -> SegmentValue)
          -> (a -> BB.Builder)
          -> Gen a
          -> PropertyT m ()
testMany fmt wrap put gen = do
  vs <- forAll $ Gen.list (Range.linear 0 20) gen
  let body = vs <&> put & mconcat & BB.toLazyByteString
  let seg = segment body $ metaFormat (Known fmt)
  segmentValues seg === fmap wrap vs
