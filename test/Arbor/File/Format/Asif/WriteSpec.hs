{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}

module Arbor.File.Format.Asif.WriteSpec where

import Arbor.File.Format.Asif.Data.Ip
import Arbor.File.Format.Asif.Format
import Arbor.File.Format.Asif.Segment
import Arbor.File.Format.Asif.Write
import Control.Lens
import Control.Monad.IO.Class                (liftIO)
import Control.Monad.Trans.Resource
import Data.Int
import Data.List                             (elemIndex, nub)
import Data.Maybe                            (fromMaybe)
import Data.Semigroup                        ((<>))
import Data.Word
import HaskellWorks.Data.Network.Ip.Validity (Canonical)
import System.IO.Temp                        (openBinaryTempFile)

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Attoparsec.ByteString        as AP
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Text.Lazy                    as T
import qualified Data.Text.Lazy.Encoding           as T
import qualified Data.Thyme.Time.Core              as TY
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as IP4
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as IP6
import qualified Hedgehog.Gen                      as G
import qualified Hedgehog.Range                    as R

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
    lst <- forAll $ G.list (R.linear 0 50) genIpv4

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

  it "should write out and read back in a ipv4 block segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) genIpv4Block

    lbs <- asifContent "wxyz" Nothing (ipv4BlockSegment id "ipv4Block") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SIpv4Block <$> lst) === seg

  it "should write out and read back in a ipv6 block segment" $ require $ property $ do
    lst <- forAll $ G.list (R.linear 0 50) genIpv6Block

    lbs <- asifContent "wxyz" Nothing (ipv6BlockSegment id "ipv6Block") lst

    let Right segments = extractSegments (AP.string "seg:wxyz") lbs
    [names, times, types, seg] <- forAll $ pure (segmentValues <$> segments)

    (SIpv6Block <$> lst) === seg

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

  it "should write and read back a lookup segment" $ require $ property $ do
    let pairGen = (,) <$> genIpv4 <*> G.maybe (G.element ["US", "KZ", "AU"])
    pairs <- forAll $ G.list (R.linear 1 100) pairGen

    let countriesFold = nullTerminatedStringSegment id "countries"
    let countriesLookup = word16LookupSegment "ip-to-countries" snd countriesFold

    let resFold = ipv4Segment fst "ip" <> countriesLookup

    content <- asifContent "ipct" Nothing resFold pairs

    case extractSegments (AP.string "seg:ipct") content of
      Right segments -> do
        [names, times, types, ips, lkp, dict] <- forAll $ pure (segmentValues <$> segments)

        let expectedVals    = pairs ^.. each . _2 . _Just & nub
        let expectedLkpVals = pairs ^.. each . _2 . to (\x -> x >>= flip elemIndex expectedVals) . to (fromMaybe maxBound)

        expectedDict <- forAll . pure $
          expectedVals <&> (SString . T.encodeUtf8 . T.fromStrict)

        expectedLkp <- forAll . pure $
          expectedLkpVals <&> (SWord16 . fromIntegral)

        dict === expectedDict
        lkp  === expectedLkp
      Left _ -> failure


genTriple :: (MonadGen m, GenBase m ~ Identity) => m (Int64, Word16, T.Text)
genTriple
  = (,,)
  <$> G.int64 R.linearBounded
  <*> G.word16 R.linearBounded
  <*> genNonNullText

genIpv4 :: MonadGen m => m IP4.IpAddress
genIpv4 = word32ToIpv4 <$> G.word32 R.linearBounded

genIpv6 :: MonadGen m => m IP6.IpAddress
genIpv6 = word32x4ToIpv6 <$> gen4
  where
    gen4 :: MonadGen m => m (Word32, Word32, Word32, Word32)
    gen4 =
      let g = G.word32 R.linearBounded
      in (,,,) <$> g <*> g <*> g <*> g

genIpv4Block :: MonadGen m => m (IP4.IpBlock Canonical)
genIpv4Block = do
  ip <- genIpv4
  mask <- IP4.IpNetMask <$> G.word8 (R.linear 0 32)
  pure . IP4.canonicaliseIpBlock $ IP4.IpBlock ip mask

genIpv6Block :: MonadGen m => m (IP6.IpBlock Canonical)
genIpv6Block = do
  ip <- genIpv6
  mask <- IP6.IpNetMask <$> G.word8 (R.linear 0 128)
  pure . IP6.canonicaliseIpBlock $ IP6.IpBlock ip mask

#if MIN_VERSION_hedgehog(1, 0, 0)
genNonNullText :: (MonadGen m, GenBase m ~ Identity) => m T.Text
genNonNullText =
  T.fromStrict <$> G.text (R.linear 0 32) (G.filter (/= toEnum 0) G.unicode)
#else
#endif

#if MIN_VERSION_hedgehog(1, 0, 0)
#else
instance MonadResource m => MonadResource (PropertyT m) where
  liftResourceT = lift . liftResourceT
#endif

-- I have no idea how this doesn't exist, but whatever. ¯\_(ツ)_/¯
instance MonadResource IO where
  liftResourceT = runResourceT
