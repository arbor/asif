{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arbor.File.Format.Asif.Format.TextSpec
  ( spec
  ) where

import           Arbor.File.Format.Asif.Data.Ip
import           Arbor.File.Format.Asif.Format      as F
import           Arbor.File.Format.Asif.Format.Text as F
import           Arbor.File.Format.Asif.Type
import           Arbor.File.Format.Asif.Whatever
import           Control.Lens
import qualified Data.List                          as L
import           Data.Semigroup                     ((<>))
import           Data.Thyme.Time.Core
import           HaskellWorks.Data.Bits.BitShow     (bitShow)

import qualified Data.ByteString.Builder            as BB
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Lazy.Char8         as LC8
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T

import           Arbor.TestUtils
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec


import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "Arbor.File.Format.Asif.Format.TextSpec" $ do
  it "should parse strings" $ require $ property $ do
    ts <- forAll $ Gen.list (Range.linear 0 20) (Gen.text (Range.linear 0 512) Gen.alphaNum)
    let body = ts <&> T.encodeUtf8 <&> (<> "\0") & mconcat & LBS.fromStrict
    let seg = segment body $ metaFormat (Known F.StringZ)
    F.extractValues F.segmentValueToText seg === ts

  it "should parse chars" $ require $ property $ do
    txt <- forAll $ Gen.text (Range.linear 0 512) Gen.alphaNum
    let expected = T.unpack txt <&> T.singleton
    let body = txt & T.encodeUtf8 & LBS.fromStrict
    let seg = segment body $ metaFormat (Known F.Char)
    F.extractValues F.segmentValueToText seg === expected

  it "should parse time (micros)" $ require $ property $ do
    ts <- forAll $ Gen.list (Range.linear 0 20) (Gen.int64 (Range.linear 0 maxBound))
    let body = ts <&> BB.int64LE & mconcat & BB.toLazyByteString
    let times = ts ^.. each . from microseconds . to posixSecondsToUTCTime
    let seg = segment body $ metaFormat (Known F.TimeMicros64LE)
    F.extractValues F.segmentValueToText seg === (T.pack . show <$> times)

  it "should parse time (millis)" $ require $ property $ do
    ts <- forAll $ Gen.list (Range.linear 0 20) (Gen.int64 (Range.linear 0 (maxBound `div` 1000)))
    let body = ts <&> BB.int64LE & mconcat & BB.toLazyByteString
    let times = ts ^.. each . to (*1000) . from microseconds . to posixSecondsToUTCTime
    let seg = segment body $ metaFormat (Known F.TimeMillis64LE)
    F.extractValues F.segmentValueToText seg === (T.pack . show <$> times)

  it "should parse int64" $ require $ property $ do
    testMany F.Int64LE (T.pack . show) BB.int64LE (Gen.int64 (Range.linear 0 maxBound))

  it "should parse int32" $ require $ property $ do
    testMany F.Int32LE (T.pack . show) BB.int32LE (Gen.int32 (Range.linear 0 maxBound))

  it "should parse int16" $ require $ property $ do
    testMany F.Int16LE (T.pack . show) BB.int16LE (Gen.int16 (Range.linear 0 maxBound))

  it "should parse int8" $ require $ property $ do
    testMany F.Int8 (T.pack . show) BB.int8 (Gen.int8 (Range.linear 0 maxBound))

  it "should parse Word64" $ require $ property $ do
    testMany F.Word64LE (T.pack . show) BB.word64LE (Gen.word64 (Range.linear 0 maxBound))

  it "should parse Word32" $ require $ property $ do
    testMany F.Word32LE (T.pack . show) BB.word32LE (Gen.word32 (Range.linear 0 maxBound))

  it "should parse Word16" $ require $ property $ do
    testMany F.Word16LE (T.pack . show) BB.word16LE (Gen.word16 (Range.linear 0 maxBound))

  it "should parse Word8" $ require $ property $ do
    testMany F.Word8 (T.pack . show) BB.word8 (Gen.word8 (Range.linear 0 maxBound))

  it "should parse BitString" $ require $ property $
    let gen = Gen.bytes (Range.linear 0 256) <&> LBS.fromStrict
    in testOne F.BitString (T.pack . bitShow) BB.lazyByteString gen

  it "should parse Bitmap" $ require $ property $
    let gen = Gen.bytes (Range.linear 0 256) <&> LBS.fromStrict
    in testOne F.Bitmap (T.pack . bitShow) BB.lazyByteString gen

  it "should parse repeated BitString" $ require $ property $ do
    n <- forAll $ Gen.int (Range.linear 1 32)
    vs <- forAll $ Gen.list (Range.linear 0 20) (Gen.bytes (Range.singleton n) <&> LBS.fromStrict)
    let seg = segment (mconcat vs) $ metaFormat (Known (Repeat (fromIntegral n) F.BitString))
    F.extractValues F.segmentValueToText seg === fmap (T.pack . bitShow) vs

  it  "should parse repeated chars" $ require $ property $ do
    n <- forAll $ Gen.int (Range.linear 1 32)
    vs <- forAll $ Gen.list (Range.linear 0 20) (Gen.string (Range.singleton n) Gen.alphaNum)
    let seg = segment (LC8.pack $ mconcat vs) $ metaFormat (Known (Repeat (fromIntegral n) F.Char))
    F.extractValues F.segmentValueToText seg === fmap (T.pack . L.intersperse ',') vs

  it "should parse repeated ips" $ require $ property $ do
    n <- forAll $ Gen.int (Range.linear 1 32)
    k <- forAll $ Gen.int (Range.linear 0 20)
    vs <- forAll $ Gen.list (Range.singleton (n*k)) (Gen.word32 Range.linearBounded)
    let body = vs <&> BB.word32LE & mconcat & BB.toLazyByteString
    let seg = segment body $ metaFormat (Known (Repeat (fromIntegral n) F.Ipv4))
    let expected = vs <&> (ipv4ToString . word32ToIpv4) & chunksOf n <&> (T.pack . L.intercalate ",")
    F.extractValues F.segmentValueToText seg === expected

testMany :: (Monad m, Show a)
          => Format
          -> (a -> T.Text)
          -> (a -> BB.Builder)
          -> Gen a
          -> PropertyT m ()
testMany fmt wrap put gen = do
  vs <- forAll $ Gen.list (Range.linear 0 20) gen
  let body = vs <&> put & mconcat & BB.toLazyByteString
  let seg = segment body $ metaFormat (Known fmt)
  F.extractValues F.segmentValueToText seg === fmap wrap vs

testOne :: (Monad m, Show a)
        => Format
        -> (a -> T.Text)
        -> (a -> BB.Builder)
        -> Gen a
        -> PropertyT m ()
testOne fmt wrap put gen = do
  v <- forAll gen
  let body = put v & BB.toLazyByteString
  let seg = segment body $ metaFormat (Known fmt)
  F.extractValues F.segmentValueToText seg === [wrap v]
