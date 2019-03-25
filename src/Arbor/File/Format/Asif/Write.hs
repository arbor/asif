{-# LANGUAGE MultiParamTypeClasses #-}


-- |
-- Module: Arbor.File.Format.Asif.Write
--
-- Functions to make it easier to write ASIF files without needing to deal with
-- raw Handles and ByteStrings too much.

module Arbor.File.Format.Asif.Write
  (
  -- * Encode an entire ASIF bytestring
  -- $usage
    writeAsif
  , asifContent
  , asifContentC

  -- * Folds for Segments
  -- $segments
  , lazyByteStringSegment
  , nullTerminatedStringSegment
  , textSegment
  , asciiSegment
  , boolSegment
  , word8Segment
  , word16Segment
  , word32Segment
  , word64Segment
  , int8Segment
  , int16Segment
  , int32Segment
  , int64Segment
  , ipv4Segment
  , ipv6Segment
  , ipv4BlockSegment
  , ipv6BlockSegment
  , utcTimeMicrosSegment

  -- * Utility functions
  -- $helper
  , genericInitial
  , genericStep
  , genericExtract
  , genericFold
  )
where

import Arbor.File.Format.Asif.ByteString.Builder
import Arbor.File.Format.Asif.Data.Ip            (ipv4ToWord32, ipv6ToWord32x4)
import Arbor.File.Format.Asif.Type
import Arbor.File.Format.Asif.Whatever           (Whatever (..))
import Conduit
import Control.Foldl
import Control.Lens
import Control.Monad.IO.Class                    (liftIO)
import Control.Monad.Trans.Resource              (MonadResource)
import Data.Int
import Data.Profunctor                           (lmap)
import Data.Semigroup                            ((<>))
import Data.Word
import HaskellWorks.Data.Network.Ip.Validity     (Canonical (..))
import System.IO                                 (Handle, hFlush)
import System.IO.Temp                            (openTempFile)

import qualified Arbor.File.Format.Asif.Format     as F
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Builder           as BB
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.Encoding           as TE
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as IP4
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as IP6

import qualified Data.Thyme.Clock.POSIX as TY
import qualified Data.Thyme.Time.Core   as TY

-- $usage
--
-- Facilities for writing an entire ASIF file, either to an actual file or to a
-- ByteString.
--
-- ** Usage:
-- Use the various *segment functions to produce a FoldM. These are designed to
-- allow you to take some large type (e.g. a tuple or a product type) and pull
-- out the constituent pieces to encode into 'Segment's.
--
-- 'FoldM's are composable using '<>'. Once you have a single, provide it to
-- 'writeAsif' or 'asifContent' along with an appropriate foldable.
-- Both these functions will stream from the input, assuming the foldable is
-- something that can be streamed from.

-- | Write an ASIF file to the supplied handle.
-- Streams the input foldable if possible.
writeAsif :: (Foldable f, MonadResource m)
  => Handle
  -> String
  -> Maybe TY.POSIXTime
  -> FoldM m a [Segment Handle]
  -> f a
  -> m ()
writeAsif hOutput asifType mTimestamp fld foldable = do
  runConduit
    $  asifContentC asifType mTimestamp fld foldable
    .| sinkHandle hOutput
  liftIO $ hFlush hOutput


-- | Builds a lazy ASIF bytestring.
-- Streams the input foldable if possible.
asifContent :: (Foldable f, MonadResource m)
  => String
  -> Maybe TY.POSIXTime
  -> FoldM m a [Segment Handle]
  -> f a
  -> m LBS.ByteString
asifContent asifType mTimestamp fld foldable =
  runConduit
    $  asifContentC asifType mTimestamp fld foldable
    .| sinkLazy

-- | Returns ASIF content as a conduit
asifContentC :: (Foldable f, MonadResource m)
  => String
  -> Maybe TY.POSIXTime
  -> FoldM m a [Segment Handle]
  -> f a
  -> ConduitT () BS.ByteString m ()
asifContentC asifType mTimestamp fld foldable = do
  segments <- lift $ foldM fld foldable
  segmentsC asifType mTimestamp segments

----
-- $segments
--
-- Use these to build 'FoldM's for the types you want to encode.
-- Compose them together using '<>'.

-- | Builds a segment from lazy bytestrings.
-- This can in priciple cover any bytestring-y format,
-- including StringZ, Text, Binary, Bitmap, and Bitstring, as well as unknown encodings.
-- Correctly encoding the value is the responsibility of the caller.
lazyByteStringSegment :: MonadResource m => Whatever F.Format -> (a -> LBS.ByteString) -> T.Text -> FoldM m a [Segment Handle]
lazyByteStringSegment = genericFold BB.lazyByteString

-- | Builds a segment of null-termianted strings.
-- Note that the input itself does *not* need to be null-terminated.
-- The null-termination is added by this function.
nullTerminatedStringSegment :: MonadResource m => (a -> T.Text) -> T.Text -> FoldM m a [Segment Handle]
nullTerminatedStringSegment f t = FoldM step initial extract
  where
    initial = genericInitial t
    step h b = do
      liftIO $ BB.hPutBuilder h $ BB.byteString (T.encodeUtf8 . f $ b) <> BB.word8 0
      pure h
    extract = genericExtract t (Known F.StringZ)

-- | Builds a segment of 'Text's.
textSegment :: MonadResource m => (a -> T.Text) -> T.Text -> FoldM m a [Segment Handle]
textSegment f = genericFold TE.encodeUtf8Builder (Known F.Text) (TL.fromStrict . f)

-- | Builds a segment of 'Char's.
asciiSegment :: MonadResource m => (a -> Char) -> T.Text -> FoldM m a [Segment Handle]
asciiSegment = genericFold BB.char8 (Known F.Char)

-----

-- | Build a segment of 'Bool's, encoded as 'Word8's, where False == 0, and True == 1
boolSegment :: MonadResource m => (a -> Bool) -> T.Text -> FoldM m a [Segment Handle]
boolSegment f = genericFold BB.word8 (Known F.Bool) (bool2word8 . f)
    where
      bool2word8 False = 0
      bool2word8 True  = 1

-- | Builds a segment of 'Word8's.
word8Segment :: MonadResource m => (a -> Word8) -> T.Text -> FoldM m a [Segment Handle]
word8Segment = genericFold BB.word8 (Known F.Word8)

-- | Builds a segment of 'Word16's.
word16Segment :: MonadResource m => (a -> Word16) -> T.Text -> FoldM m a [Segment Handle]
word16Segment = genericFold BB.word16LE (Known F.Word16LE)

-- | Builds a segment of 'Word32's.
word32Segment :: MonadResource m => (a -> Word32) -> T.Text -> FoldM m a [Segment Handle]
word32Segment = genericFold BB.word32LE (Known F.Word32LE)

-- | Builds a segment of 'Word64's.
word64Segment :: MonadResource m => (a -> Word64) -> T.Text -> FoldM m a [Segment Handle]
word64Segment = genericFold BB.word64LE (Known F.Word64LE)

-----

-- | Builds a segment of 'Int8's.
int8Segment :: MonadResource m => (a -> Int8) -> T.Text -> FoldM m a [Segment Handle]
int8Segment = genericFold BB.int8 (Known F.Int8)

-- | Builds a segment of 'Int16's.
int16Segment :: MonadResource m => (a -> Int16) -> T.Text -> FoldM m a [Segment Handle]
int16Segment = genericFold BB.int16LE (Known F.Int16LE)

-- | Builds a segment of 'Int32's.
int32Segment :: MonadResource m => (a -> Int32) -> T.Text -> FoldM m a [Segment Handle]
int32Segment = genericFold BB.int32LE (Known F.Int32LE)

-- | Builds a segment of 'Int64's.
int64Segment :: MonadResource m => (a -> Int64) -> T.Text -> FoldM m a [Segment Handle]
int64Segment = genericFold BB.int64LE (Known F.Int64LE)

-----

-- | Builds a segment of 'IPv4's.
ipv4Segment :: MonadResource m => (a -> IP4.IpAddress) -> T.Text -> FoldM m a [Segment Handle]
ipv4Segment f = genericFold BB.word32LE (Known F.Ipv4) (ipv4ToWord32 . f)

-- | Builds a segment of 'IPv6's.
ipv6Segment :: MonadResource m => (a -> IP6.IpAddress) -> T.Text -> FoldM m a [Segment Handle]
ipv6Segment f = genericFold encoding (Known F.Ipv6) extract
    where
      -- I do not know why this is Big-Endian, when everything else is Little-Endian.
      encoding = Prelude.foldMap BB.word32BE
      extract = tupleToList . ipv6ToWord32x4 . f

-- | Builds a segment of IPv4 CIDR blocks
ipv4BlockSegment :: MonadResource m => (a -> IP4.IpBlock Canonical) -> T.Text -> FoldM m a [Segment Handle]
ipv4BlockSegment = genericFold encoding (Known F.Ipv4Block)
    where
      encoding (IP4.IpBlock (IP4.IpAddress ip) (IP4.IpNetMask block)) = BB.word32LE ip <> BB.word8 block

-- | Builds a segment of IPv6 CIDR blocks
ipv6BlockSegment :: MonadResource m => (a -> IP6.IpBlock Canonical) -> T.Text -> FoldM m a [Segment Handle]
ipv6BlockSegment = genericFold encoding (Known F.Ipv6Block)
    where
      encoding (IP6.IpBlock ip (IP6.IpNetMask block)) =
        let extract = Prelude.foldMap BB.word32BE . tupleToList . ipv6ToWord32x4
        in extract ip <> BB.word8 block

-----

-- | Builds a segment of 'UTCTime's, accurate to microseconds.
utcTimeMicrosSegment :: MonadResource m => (a -> TY.UTCTime) -> T.Text -> FoldM m a [Segment Handle]
utcTimeMicrosSegment f = genericFold BB.int64LE (Known F.TimeMicros64LE) (fromTime . f)
    where
      fromTime :: TY.UTCTime -> Int64
      fromTime = view (TY.posixTime . TY.microseconds)

-------------------------------------------------------------------------------

-- $helper
--
-- Helper functions for creating 'FoldM's from scratch.

genericInitial :: MonadResource m => T.Text -> m Handle
genericInitial name = do
  (_, _, h) <- openTempFile Nothing (T.unpack name)
  pure h

genericStep :: MonadResource m => (a -> BB.Builder) -> Handle -> a -> m Handle
genericStep enc h b = do
  liftIO $ BB.hPutBuilder h $ enc b
  pure h

genericExtract :: MonadResource m => T.Text -> Whatever F.Format -> Handle -> m [Segment Handle]
genericExtract filen typ h = pure [segment h $ metaFilename filen <> metaFormat typ]

genericFold :: MonadResource m =>  (a -> BB.Builder) -> Whatever F.Format -> (b -> a) -> T.Text -> FoldM m b [Segment Handle]
genericFold enc fmt f t = lmap f $ FoldM (genericStep enc) (genericInitial t) (genericExtract t fmt)

-------------------------------------------------------------------------------

-- Private

tupleToList :: (a,a,a,a) -> [a]
tupleToList (w1,w2,w3,w4) = [w1,w2,w3,w4]
