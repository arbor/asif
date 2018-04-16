{-# LANGUAGE ScopedTypeVariables #-}

module SegmentedFormatSpec
( spec
)
where

import Arbor.Network.Ip
import Control.Lens
import Control.Monad.IO.Class
import Data.Either                 (isLeft, isRight)
import Data.Maybe                  (fromJust)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.List               as DL
import qualified Data.Map.Strict         as M
import qualified Data.Vector.Unboxed     as VU
import qualified SegmentedFormat         as C
import qualified Gen.Feed                as G
import qualified Hedgehog.Gen            as G
import qualified Hedgehog.Range          as R

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "GAN.ColumnarFeed" $ do
  it "should extract segments" $ require $ withTests 1 $ property $ do
    bs <- liftIO $ LBS.readFile "test/resources/geo_countries_columnar.columnar"
    isRight (C.extractSegments "seg:ganc" bs) === True

  it "should extract start + stop cidrs" $ require $ withTests 1 $ property $ do
    bs <- liftIO $ LBS.readFile "test/resources/geo_countries_columnar.columnar"
    let segs = C.extractSegments "seg:ganc" bs
    isRight segs === True

    let Right [a, b, _, _, _] = segs
    let starts = C.segmentCidrs a
    let stops = C.segmentCidrs b

    VU.length starts === 26
    VU.length stops === 26

  it "should extract CCs" $ require $ withTests 1 $ property $ do
    bs <- liftIO $ LBS.readFile "test/resources/geo_countries_columnar.columnar"
    let segs = C.extractSegments "seg:ganc" bs
    let Right [_, _, seg, _, _] = segs
    let ccs = C.segmentCodes seg
    VU.length ccs === 26
    ccs VU.! 0 === ('A', 'U')

  it "should extract identifiers" $ require $ withTests 1 $ property $ do
    bs <- liftIO $ LBS.readFile "test/resources/geo_asns_columnar.columnar"
    let segs = C.extractSegments "seg:gana" bs
    let Right [_, _, seg, _, _] = segs
    let naicIDs = C.segmentIdentifiers seg
    VU.length naicIDs === 52
    naicIDs VU.! 0 === 13335

  it "should read maps" $ require $ withTests 1 $ property $ do
    bs <- liftIO $ LBS.readFile "test/resources/geo_countries_columnar.columnar"
    let segs = C.extractSegments "seg:ganc" bs
    let Right [_, _, _, ks, vs] = segs
    let map = C.getCountryCodeMap ks vs

    M.lookup ('C', 'N') map === Just "China"

  it "should search for cidr hits" $ require $ withTests 1 $ property $ do
    bsCcs <- liftIO $ LBS.readFile "test/resources/geo_countries_columnar.columnar"
    let (Right feed) = C.loadCountryFeed bsCcs
    C.lookupCidr feed 16778240 === Just 3
    C.lookupCidr feed 26777473 === Nothing

  it "should reject input files with mismatched segments" $ require $ withTests 1 $ property $ do
    bsCcs2 <- liftIO $ LBS.readFile "test/resources/geo_countries_columnar_not_matching_segments.columnar"
    isLeft (C.loadCountryFeed bsCcs2) === True

  it "should find identifiers from cidrs" $ require $ withTests 1 $ property $ do
    bsCcs <- liftIO $ LBS.readFile "test/resources/geo_countries_columnar.columnar"
    let (Right feed) = C.loadCountryFeed bsCcs
    let i = C.lookupIdentifier feed 16778240
    i === Just ('A', 'U')

  it "should get names from identifiers" $ require $ withTests 1 $ property $ do
    bs <- liftIO $ LBS.readFile "test/resources/geo_countries_columnar.columnar"
    let (Right feed) = C.loadCountryFeed bs

    let name = C.lookupValue feed 16778240 C.names
    name === Just "Australia"

  it "should query correctly via binarySearch" $ require $ property $ do
    ip1 <- forAll $ G.ipv4 128 195
    ip2 <- forAll $ G.ipv4 0 127
    ip3 <- forAll $ G.ipv4 196 255
    mid   <- forAll $ G.feedElement (ipToWord32 ip1) (ipToWord32 ip1 + 100)
    left  <- filter (\x -> (x ^. _1) /= (mid ^. _1)) <$> forAll (G.feedElements (ipToWord32 ip2) (ipToWord32 ip2 + 1000))
    right <- filter (\x -> (x ^. _1) /= (mid ^. _1)) <$> forAll (G.feedElements (ipToWord32 ip3) (ipToWord32 ip3 + 1000))
    let v = VU.fromList $ DL.sortBy (G.ordBy (^. _1)) (left DL.++ [mid] DL.++ right)
    let (Just idx1) = C.binarySearch mid v
    mid === v VU.! idx1
    let (Just idx2) = C.binarySearch ((mid ^. _1) + 10, mid ^. _2, mid ^. _3) v
    mid === v VU.! idx2
    let (Just idx3) = C.binarySearch ((mid ^. _1) + 100, mid ^. _2, mid ^. _3) v
    mid === v VU.! idx3
