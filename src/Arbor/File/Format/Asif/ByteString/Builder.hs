{-# LANGUAGE BangPatterns #-}

module Arbor.File.Format.Asif.ByteString.Builder
  ( magicString
  , withSize
  , segmentsC
  , segmentsRawC
  , makeMagic
  , magicLength
  ) where

import Arbor.File.Format.Asif.Text
import Arbor.File.Format.Asif.Type
import Arbor.File.Format.Asif.Whatever
import Conduit
import Control.Lens
import Control.Monad
import Data.Bits
import Data.ByteString.Builder
import Data.Conduit                    (Source)
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Text                       (Text)
import Data.Thyme.Clock
import Data.Thyme.Clock.POSIX          (POSIXTime, getPOSIXTime)
import Data.Word

import qualified Arbor.File.Format.Asif.ByteString as BS
import qualified Arbor.File.Format.Asif.Format     as F
import qualified Arbor.File.Format.Asif.IO         as IO
import qualified Arbor.File.Format.Asif.Lens       as L
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Builder           as B
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.ByteString.Lazy.Char8        as LC8
import qualified Data.Conduit.List                 as CL
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified GHC.IO.Handle                     as IO
import qualified System.IO                         as IO hiding (openTempFile)
import qualified System.IO.Temp                    as IO

makeMagic :: String -> Builder
makeMagic c = B.lazyByteString (magicString c)

magicPrefix :: LBS.ByteString
magicPrefix = "seg:"

-- magic file identifier for segmented gan feeds.
-- 7 characters. the 8th is meant to be filled in based on feed.
magicString :: String -> LC8.ByteString
magicString s = if LBS.length truncatedMagic < LBS.length rawMagic
  then truncatedMagic
  else error $ "Magic length of " <> show (LC8.unpack truncatedMagic) <> " cannot be greater than " <> show magicLength
  where rawMagic        = LC8.pack "seg:" <> LC8.pack s <> LBS.replicate 12 0
        truncatedMagic  = LBS.take magicLength rawMagic

magicLength :: Int64
magicLength = 16

padding64 :: Int64 -> Int64
padding64 s = (8 - s) `mod` 8

withSize :: LBS.ByteString -> (Int64, LBS.ByteString)
withSize bs = (LBS.length bs, bs)

versionLength :: Int64
versionLength = fromIntegral $ finiteBitSize (0 :: Word64) `quot` 8

headerLen :: Int64 -> Int64
headerLen n = w64 + magicLength + n * w64
  where w64 :: Int64
        w64 = fromIntegral $ finiteBitSize (0 :: Word64) `quot` 8

intersperse :: Int64 -> Int64 -> B.Builder
intersperse a b = B.word32LE (fromIntegral a) <> B.word32LE (fromIntegral b)

segmentsRawC :: MonadIO m => String -> [IO.Handle] -> Source m BS.ByteString
segmentsRawC asifType handles = do
  let segmentCount = fromIntegral $ length handles :: Int64

  rawSizes <- forM handles $ liftIO . IO.hGetAndResetOffset
  let paddings    = padding64 <$> rawSizes
  let paddedSizes = uncurry (+) <$> zip rawSizes paddings

  let offsets = (+ headerLen segmentCount) <$> init (scanl (+) 0 paddedSizes)
  let positions    = zip offsets rawSizes

  CL.sourceList
    [ LBS.toStrict . B.toLazyByteString $ makeMagic asifType
      <> B.word64LE (fromIntegral segmentCount)           -- seg num
      <> mconcat (uncurry intersperse <$> positions)
    ]

  forM_ (zip paddings handles) $ \(padding, h) -> do
    sourceHandle h
    CL.sourceList (replicate (fromIntegral padding) (BS.singleton 0))

segmentsC :: (MonadIO m, MonadResource m)
  => String
  -> Maybe POSIXTime
  -> [Segment IO.Handle]
  -> m (Source m BS.ByteString)
segmentsC asifType maybeTimestamp metas = do
  fileTime <- maybe (liftIO getPOSIXTime) return maybeTimestamp
  (_, _, hFilenames   ) <- IO.openTempFile Nothing "asif-filenames"
  (_, _, hCreateTimes ) <- IO.openTempFile Nothing "asif-timestamps"
  (_, _, hFormats     ) <- IO.openTempFile Nothing "asif-formats"

  let metaMeta        = metaCreateTime fileTime
  let metaFilenames   = segment hFilenames    $ metaMeta <> metaFilename ".asif/filenames"   <> metaFormat (Known F.StringZ)
  let metaCreateTimes = segment hCreateTimes  $ metaMeta <> metaFilename ".asif/createtimes" <> metaFormat (Known F.TimeMicros)
  let metaFormats     = segment hFormats      $ metaMeta <> metaFilename ".asif/formats"     <> metaFormat (Known F.StringZ)
  let moreMetas       = metaFilenames:metaCreateTimes:metaFormats:metas

  forM_ moreMetas $ \meta -> do
    liftIO $ B.hPutBuilder hFilenames   $ B.byteString (meta ^. L.meta . L.filename & fromMaybe "" & T.encodeUtf8) <> B.word8 0
    liftIO $ B.hPutBuilder hCreateTimes $ B.int64LE $ (meta ^. L.meta . L.createTime) <&> (^. microseconds) & fromMaybe 0
    liftIO $ B.hPutBuilder hFormats     $ B.byteString (meta ^. L.meta . L.format <&> tShowWhatever & fromMaybe "" & T.encodeUtf8) <> B.word8 0
    return ()

  let source = segmentsRawC asifType ((^. L.payload) <$> moreMetas)

  return source
