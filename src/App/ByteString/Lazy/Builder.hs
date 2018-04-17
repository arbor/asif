module App.ByteString.Lazy.Builder
  ( magicString
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

magicLength' :: Int64
magicLength' = 8

padding64' :: Int64 -> Int64
padding64' s = (8 - s) `mod` 8

withSize :: LBS.ByteString -> (Int64, LBS.ByteString)
withSize bs = (LBS.length bs, bs)

versionLength' :: Int64
versionLength' = fromIntegral $ finiteBitSize (0 :: Word64) `quot` 8

headerLen' :: Int64 -> Int64
headerLen' n = w64 + magicLength' + versionLength' + n * w64
  where w64 :: Int64
        w64 = fromIntegral $ finiteBitSize (0 :: Word64) `quot` 8

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
