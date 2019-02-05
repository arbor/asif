{-# LANGUAGE MultiParamTypeClasses #-}

module Arbor.File.Format.Asif.Write

where

import Arbor.File.Format.Asif.ByteString.Builder
import Arbor.File.Format.Asif.Type
import Arbor.File.Format.Asif.Whatever           (Whatever (..))
import Conduit
import Control.Foldl
import Control.Lens
import Control.Monad.IO.Class                    (liftIO)
import Control.Monad.Trans.Resource              (MonadResource)
import Data.Int
import Data.Semigroup                            ((<>))
import Data.Word
import System.IO                                 (Handle, SeekMode (AbsoluteSeek), hFlush, hSeek)
import System.IO.Temp                            (openTempFile)

import qualified Arbor.File.Format.Asif.Format as F
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.IP                       as IP
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TE

import qualified Data.Thyme.Clock.POSIX as TY
import qualified Data.Thyme.Time.Core   as TY

writeAsif :: (Foldable f, MonadResource m)
  => Handle
  -> String
  -> Maybe TY.POSIXTime
  -> FoldM m a [Segment Handle]
  -> f a
  -> m ()
writeAsif hOutput asifType mTimestamp fld foldable = do
  segments <- foldM fld foldable
  contents  <- segmentsC asifType mTimestamp segments
  runConduit $ contents .| sinkHandle hOutput
  liftIO $ hFlush hOutput

buildAsifBytestring :: (Foldable f, MonadResource m)
  => String
  -> Maybe TY.POSIXTime
  -> FoldM m a [Segment Handle]
  -> f a
  -> m LBS.ByteString
buildAsifBytestring asifType mTimestamp fld foldable = do
  (_, _, h) <- openTempFile Nothing "asif"
  writeAsif h asifType mTimestamp fld foldable
  liftIO $ hSeek h AbsoluteSeek 0
  liftIO $ LBS.hGetContents h

-----

-- This can in priciple cover all of StringZ, Text, Binary, Bitmap, and Bitstring, as well as unknown encodings.
-- Correctly encoding the value is the responsibility of the caller.
lazyByteStringSegment :: MonadResource m => Whatever F.Format -> (a -> LBS.ByteString) -> T.Text -> FoldM m a [Segment Handle]
lazyByteStringSegment fmt f t = FoldM step initial extract
  where
    initial = genericInitial t
    step = genericStep BB.lazyByteString f
    extract = genericExtract t fmt

textSegment :: MonadResource m => (a -> T.Text) -> T.Text -> FoldM m a [Segment Handle]
textSegment f t = FoldM step initial extract
  where
    initial = genericInitial t
    step = genericStep TE.encodeUtf8Builder (TL.fromStrict . f)
    extract = genericExtract t (Known F.Text)

charSegment :: MonadResource m => (a -> Char) -> T.Text -> FoldM m a [Segment Handle]
charSegment f t = FoldM step initial extract
  where
    initial = genericInitial t
    step = genericStep BB.charUtf8 f
    extract = genericExtract t (Known F.Char)

-----

word8Segment :: MonadResource m => (a -> Word8) -> T.Text -> FoldM m a [Segment Handle]
word8Segment f t = FoldM step initial extract
    where
      initial = genericInitial t
      step = genericStep BB.word8 f
      extract = genericExtract t (Known F.Word8)

word16Segment :: MonadResource m => (a -> Word16) -> T.Text -> FoldM m a [Segment Handle]
word16Segment f t = FoldM step initial extract
    where
      initial = genericInitial t
      step = genericStep BB.word16LE f
      extract = genericExtract t (Known F.Word16LE)

word32Segment :: MonadResource m => (a -> Word32) -> T.Text -> FoldM m a [Segment Handle]
word32Segment f t = FoldM step initial extract
    where
      initial = genericInitial t
      step = genericStep BB.word32LE f
      extract = genericExtract t (Known F.Word32LE)

word64Segment :: MonadResource m => (a -> Word64) -> T.Text -> FoldM m a [Segment Handle]
word64Segment f t = FoldM step initial extract
    where
      initial = genericInitial t
      step = genericStep BB.word64LE f
      extract = genericExtract t (Known F.Word64LE)

-----

int8Segment :: MonadResource m => (a -> Int8) -> T.Text -> FoldM m a [Segment Handle]
int8Segment f t = FoldM step initial extract
    where
      initial = genericInitial t
      step = genericStep BB.int8 f
      extract = genericExtract t (Known F.Int8)

int16Segment :: MonadResource m => (a -> Int16) -> T.Text -> FoldM m a [Segment Handle]
int16Segment f t = FoldM step initial extract
    where
      initial = genericInitial t
      step = genericStep BB.int16LE f
      extract = genericExtract t (Known F.Int16LE)

int32Segment :: MonadResource m => (a -> Int32) -> T.Text -> FoldM m a [Segment Handle]
int32Segment f t = FoldM step initial extract
    where
      initial = genericInitial t
      step = genericStep BB.int32LE f
      extract = genericExtract t (Known F.Int32LE)

int64Segment :: MonadResource m => (a -> Int64) -> T.Text -> FoldM m a [Segment Handle]
int64Segment f t = FoldM step initial extract
    where
      initial = genericInitial t
      step = genericStep BB.int64LE f
      extract = genericExtract t (Known F.Int64LE)

-----

ipv4Segment :: MonadResource m => (a -> IP.IPv4) -> T.Text -> FoldM m a [Segment Handle]
ipv4Segment f t = FoldM step initial extract
    where
      initial = genericInitial t
      step = genericStep BB.word32LE (IP.toHostAddress . f)
      extract = genericExtract t (Known F.Ipv4)

ipv6Segment :: MonadResource m => (a -> IP.IPv6) -> T.Text -> FoldM m a [Segment Handle]
ipv6Segment f t = FoldM step initial extract
    where
      initial = genericInitial t
      step = genericStep (Prelude.foldMap BB.word32LE) (tupleToList . IP.toHostAddress6 . f)
      extract = genericExtract t (Known F.Ipv6)
      tupleToList (w1,w2,w3,w4) = [w1,w2,w3,w4]

-----

utcTimeMicrosSegment :: MonadResource m => (a -> TY.UTCTime) -> T.Text -> FoldM m a [Segment Handle]
utcTimeMicrosSegment f t = FoldM step initial extract
    where
      initial = genericInitial t
      step = genericStep BB.int64LE (fromTime . f)
      extract = genericExtract t (Known F.TimeMicros64LE)
      fromTime :: TY.UTCTime -> Int64
      fromTime = view (TY.posixTime . TY.microseconds)

-------------------------------------------------------------------------------
-- Helper functions

genericInitial :: MonadResource m => T.Text -> m Handle
genericInitial name = do
  (_, _, h) <- openTempFile Nothing (T.unpack name)
  pure h

genericStep :: MonadResource m => (a -> BB.Builder) -> (b -> a) -> Handle -> b -> m Handle
genericStep enc f h b = do
  liftIO $ BB.hPutBuilder h $ enc (f b)
  pure h

genericExtract :: MonadResource m => T.Text -> Whatever F.Format -> Handle -> m [Segment Handle]
genericExtract filen typ h = pure [segment h $ metaFilename filen <> metaFormat typ]
