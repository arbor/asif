module App.ByteString.Lazy.Builder
  ( magicString
  , segments
  , segment
  , padding64
  , withSize
  , segmentsC
  ) where

import Conduit
import Control.Monad
import Data.Bits
import Data.ByteString.Builder
import Data.Conduit            (Source)
import Data.Int
import Data.Monoid
import Data.Word

import qualified App.IO                     as IO
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Conduit.List          as CL
import qualified GHC.IO.Handle              as IO

magic :: Char -> Builder
magic c = B.lazyByteString (magicString c)

-- magic file identifier for segmented gan feeds.
-- 7 characters. the 8th is meant to be filled in based on feed.
magicString :: Char -> LC8.ByteString
magicString c = LC8.pack "seg:gan" <> LC8.pack [c]

-- this is fixed to 8 bytes since client code expects this.
magicLength :: Int
magicLength = 8

magicLength' :: Int64
magicLength' = 8

padding64 :: LBS.ByteString -> Int
padding64 bs = (8 - fromIntegral (LBS.length bs)) `mod` 8

padding64' :: Int64 -> Int64
padding64' s = (8 - s) `mod` 8

segment :: LBS.ByteString -> (Builder, Int)
segment bs =
  (B.lazyByteString bs <> mconcat (replicate padLength (B.word8 0)), paddedLength)
  where padLength = padding64 bs
        paddedLength = padLength + fromIntegral (LBS.length bs)

checkPositions :: [(Int64, LBS.ByteString)] -> [LBS.ByteString]
checkPositions = go 0
  where go :: Int -> [(Int64, LBS.ByteString)] -> [LBS.ByteString]
        go n ((s, bs):xs) | s == LBS.length bs = bs:go (n + 1) xs
        go n ((s, bs):_)  = error
                              $  "Invalid segment size in segment " <> show n <> ": "
                              <> show s <> " /= " <> show (LBS.length bs)
        go _ []           = []

withSize :: LBS.ByteString -> (Int64, LBS.ByteString)
withSize bs = (LBS.length bs, bs)

segments :: Int -> Char -> [(Int64, LBS.ByteString)] -> Builder
segments version identifier xs = magic identifier
  <> B.word64LE (fromIntegral version)
  <> B.word64LE (fromIntegral segmentCount)           -- seg num
  <> mconcat (uncurry intersperse <$> positions)      -- seg offsets + lengths
  <> segmentBlobs                                     -- seg data
 where
  segmentCount = length xs
  segmentInfo  = segment <$> checkPositions xs
  positions    = zip offsets (fromIntegral . fst <$> xs)
  offsets      = (+ headerLen segmentCount) <$> init (scanl (+) 0 $ snd <$> segmentInfo) -- [100, 200, 300] -> [0, 100, 300]
  segmentBlobs = mconcat (fst <$> segmentInfo)

versionLength :: Int
versionLength = finiteBitSize (0 :: Word64) `quot` 8

versionLength' :: Int64
versionLength' = fromIntegral $ finiteBitSize (0 :: Word64) `quot` 8

headerLen :: Int -> Int
headerLen n = w64 + magicLength + versionLength + n * w64
  where w64 = fromIntegral $ finiteBitSize (0 :: Word64) `quot` 8

headerLen' :: Int64 -> Int64
headerLen' n = w64 + magicLength' + versionLength' + n * w64
  where w64 :: Int64
        w64 = fromIntegral $ finiteBitSize (0 :: Word64) `quot` 8

intersperse :: Int -> Int -> B.Builder
intersperse a b = B.word32LE (fromIntegral a) <> B.word32LE (fromIntegral b)

intersperse' :: Int64 -> Int64 -> B.Builder
intersperse' a b = B.word32LE (fromIntegral a) <> B.word32LE (fromIntegral b)

segmentsC :: MonadIO m => Int -> Char -> [IO.Handle] -> Source m BS.ByteString
segmentsC version identifier handles = do
  let segmentCount = fromIntegral $ length handles :: Int64

  rawSizes <- forM handles $ liftIO . IO.hGetAndResetOffset
  let paddings    = padding64' <$> rawSizes
  let paddedSizes = uncurry (+) <$> zip rawSizes paddings

  let offsets = (+ headerLen' segmentCount) <$> init (scanl (+) 0 paddedSizes)
  let positions    = zip offsets rawSizes

  CL.sourceList
    [ LBS.toStrict . B.toLazyByteString $ magic identifier
      <> B.word64LE (fromIntegral version)
      <> B.word64LE (fromIntegral segmentCount)           -- seg num
      <> mconcat (uncurry intersperse' <$> positions)
    ]

  forM_ (zip paddings handles) $ \(padding, h) -> do
    sourceHandle h
    CL.sourceList (replicate (fromIntegral padding) (BS.singleton 0))
