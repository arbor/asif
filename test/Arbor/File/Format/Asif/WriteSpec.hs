module Arbor.File.Format.Asif.WriteSpec where

import Arbor.File.Format.Asif.Data.Ip
import Arbor.File.Format.Asif.Format
import Arbor.File.Format.Asif.Segment
import Arbor.File.Format.Asif.Write
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Data.Int
import Data.Semigroup                 ((<>))
import Data.Word
import System.IO.Temp                 (openBinaryTempFile)

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.IP                    as IP
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Encoding    as T
import qualified Data.Thyme.Clock.POSIX     as TY
import qualified Data.Thyme.Time.Core       as TY
import qualified Hedgehog.Gen               as G
import qualified Hedgehog.Range             as R

import qualified System.IO as IO


{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "Arbor.File.Format.Asif.Write" $ do
  it "should write file and read it back again" $ require $ property $ do
    tplList <- forAll $ G.list (R.linear 0 50) genTriple

    let fld =  int64Segment                (\(a,_,_) -> a) "first"
            <> word16Segment               (\(_,a,_) -> a) "second"
            <> nullTerminatedStringSegment (\(_,_,a) -> T.toStrict a) "third"

    (_, f, h) <- openBinaryTempFile Nothing "wtite-asif-test.asif"

    _ <- writeAsif h "wxyz" Nothing fld tplList
    liftIO $ IO.hSeek h IO.AbsoluteSeek 0

    lbs <- liftIO $ LBS.hGetContents h

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, l1,l2,l3] <- forAll $ pure (segmentValues <$> segments)

    ((\(a,_,_) -> SInt64 a) <$> tplList) === l1
    ((\(_,a,_) -> SWord16 a) <$> tplList) === l2
    ((\(_,_,a) -> SString . T.encodeUtf8 $ a) <$> tplList) === l3

  it "should write several segments, then read them back in" $ require $ property $ do
    tplList <- forAll $ G.list (R.linear 0 50) genTriple

    let f1 = int64Segment                (\(a,_,_) -> a) "first"
    let f2 = word16Segment               (\(_,a,_) -> a) "second"
    let f3 = nullTerminatedStringSegment (\(_,_,a) -> T.toStrict a) "third"
    let fld = f1 <> f2 <> f3

    lbs <- asifContent "wxyz" Nothing fld tplList

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs

    [names, times, types, l1,l2,l3] <- forAll $ pure (segmentValues <$> segments)

    ((\(a,_,_) -> SInt64 a) <$> tplList) === l1
    ((\(_,a,_) -> SWord16 a) <$> tplList) === l2
    ((\(_,_,a) -> SString . T.encodeUtf8 $ a) <$> tplList) === l3

-----

  it "should write out and read back in a bool segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) G.bool

    lbs <- asifContent "wxyz" Nothing (boolSegment id "bool") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SBool <$> lst) === seg

  it "should write out and read back in a word8 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.word8 R.linearBounded)

    lbs <- asifContent "wxyz" Nothing (word8Segment id "word8") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SWord8 <$> lst) === seg

  it "should write out and read back in a word16 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.word16 R.linearBounded)

    lbs <- asifContent "wxyz" Nothing (word16Segment id "word16") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SWord16 <$> lst) === seg

  it "should write out and read back in a word32 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.word32 R.linearBounded)

    lbs <- asifContent "wxyz" Nothing (word32Segment id "word32") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SWord32 <$> lst) === seg

  it "should write out and read back in a word64 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.word64 R.linearBounded)

    lbs <- asifContent "wxyz" Nothing (word64Segment id "word64") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SWord64 <$> lst) === seg

-----

  it "should write out and read back in a int8 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.int8 R.linearBounded)

    lbs <- asifContent "wxyz" Nothing (int8Segment id "int8") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SInt8 <$> lst) === seg

  it "should write out and read back in a int16 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.int16 R.linearBounded)

    lbs <- asifContent "wxyz" Nothing (int16Segment id "int16") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SInt16 <$> lst) === seg

  it "should write out and read back in a int32 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.int32 R.linearBounded)

    lbs <- asifContent "wxyz" Nothing (int32Segment id "int32") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SInt32 <$> lst) === seg

  it "should write out and read back in a int64 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (G.int64 R.linearBounded)

    lbs <- asifContent "wxyz" Nothing (int64Segment id "int64") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SInt64 <$> lst) === seg

-----

  it "should write out and read back in a ipv4 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (word32ToIpv4 <$> G.word32 R.linearBounded)

    lbs <- asifContent "wxyz" Nothing (ipv4Segment id "ipv4") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SIpv4 <$> lst) === seg

  it "should write out and read back in a ipv6 segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) genIpv6

    lbs <- asifContent "wxyz" Nothing (ipv6Segment id "ipv6") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SIpv6 <$> lst) === seg

-----

  it "should write out and read back in a time segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) (TY.posixSecondsToUTCTime . TY.fromMicroseconds <$> G.int64 R.linearBounded)

    lbs <- asifContent "wxyz" Nothing (utcTimeMicrosSegment id "time") lst
    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (STime <$> lst) === seg

-----

  it "should write out and read back in a char segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) G.ascii

    lbs <- asifContent "wxyz" Nothing (asciiSegment id "char") lst
    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SChar <$> lst) === seg

  it "should write out and read back in a null-terminated string segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) genNonNullText

    lbs <- asifContent "wxyz" Nothing (nullTerminatedStringSegment id "nullterminatedstring") (T.toStrict <$> lst)
    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SString . T.encodeUtf8 <$> lst) === seg

  it "should write out and read back in a text segment" $ require $ property $ do
    -- Explaination of this test:
    -- We can't deliniate 'breaks' in Text when we read it back in,
    -- so regardless of how many we write out, when we read it back we only
    -- get a single Text value.
    lst <- forAll $ G.list (R.linear 1 50) (G.text (R.linear 0 50) G.unicode)

    lbs <- asifContent "wxyz" Nothing (textSegment id "text") lst
    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    [SText . T.encodeUtf8 . T.concat $ T.fromStrict <$> lst] === seg


genTriple :: MonadGen m => m (Int64, Word16, T.Text)
genTriple
  = (,,)
  <$> G.int64 R.linearBounded
  <*> G.word16 R.linearBounded
  <*> genNonNullText

genIpv6 :: MonadGen m => m IP.IPv6
genIpv6 = word32x4ToIpv6 <$> gen4
  where
    gen4 :: MonadGen m => m (Word32, Word32, Word32, Word32)
    gen4 =
      let g = G.word32 R.linearBounded
      in (,,,) <$> g <*> g <*> g <*> g

genNonNullText :: MonadGen m => m T.Text
genNonNullText =
  T.fromStrict <$> G.text (R.linear 0 32) (G.filter (/= toEnum 0) G.unicode)


instance MonadResource m => MonadResource (PropertyT m) where
  liftResourceT = lift . liftResourceT

-- I have no idea how this doesn't exist, but whatever. ¯\_(ツ)_/¯
instance MonadResource IO where
  liftResourceT = runResourceT
