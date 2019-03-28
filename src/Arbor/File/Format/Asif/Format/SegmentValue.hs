{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Arbor.File.Format.Asif.Format.SegmentValue
where

import qualified Arbor.File.Format.Asif.ByteString.Lazy as LBS
import           Arbor.File.Format.Asif.Format.Decoder  as D
import qualified Arbor.File.Format.Asif.Format.Type     as F
import           Arbor.File.Format.Asif.List            as L
import           Arbor.File.Format.Asif.Segment
import           Arbor.File.Format.Asif.Whatever
import           Control.Lens
import qualified Data.Binary                            as G
import qualified Data.Binary.Get                        as G
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.ByteString.Lazy.Char8             as LBSC
import           Data.Generics.Product.Any
import           Data.Int
import           Data.Semigroup                         ((<>))
import qualified Data.Text                              as T
import           Data.Thyme.Time.Core
import           Data.Word
import           GHC.Generics                           (Generic)
import qualified HaskellWorks.Data.Network.Ip.Ipv4      as IP4
import qualified HaskellWorks.Data.Network.Ip.Ipv6      as IP6
import           HaskellWorks.Data.Network.Ip.Validity

data SegmentValue
  = SString LBS.ByteString
  | SBool Bool
  | SChar Char
  | STime UTCTime
  | SIpv4 IP4.IpAddress
  | SIpv6 IP6.IpAddress
  | SIpv4Block (IP4.IpBlock Canonical)
  | SIpv6Block (IP6.IpBlock Canonical)
  | SInt64 Int64
  | SInt32 Int32
  | SInt16 Int16
  | SInt8 Int8
  | SWord64 Word64
  | SWord32 Word32
  | SWord16 Word16
  | SWord8 Word8
  | SText LBS.ByteString
  | SBitString LBS.ByteString
  | SBitmap LBS.ByteString
  | SBinary LBS.ByteString
  | SList [SegmentValue]
  | SUnknown T.Text LBS.ByteString
  deriving (Show, Eq, Generic)

getValues :: Int64 -> G.Get a -> LBSC.ByteString -> [a]
getValues n f bs =
  let getValue bs' = G.runGet f (LBS.take n (bs' <> LBS.replicate n 0))
  in LBS.chunkBy n bs <&> getValue

getRawValue :: F.Format -> LBS.ByteString -> [SegmentValue]
getRawValue format bs =
  case format of
    F.StringZ ->
      if LBS.null bs
        then []
        else init (LBS.split 0 bs) <&> SString

    F.Bool -> whenNonEmpty bs $
      bs & getValues 1 D.getBool <&> SBool

    F.Char -> whenNonEmpty bs $
      LBSC.unpack bs <&> SChar

    F.TimeMillis64LE -> whenNonEmpty bs $
      bs & getValues 8 D.getTimeMillis <&> STime

    F.TimeMicros64LE -> whenNonEmpty bs $
      bs & getValues 8 D.getTimeMicros <&> STime

    F.Ipv4 -> whenNonEmpty bs $
      bs & getValues 4 D.getIpv4 <&> SIpv4

    F.Ipv6 -> whenNonEmpty bs $
      bs & getValues 16 D.getIpv6 <&> SIpv6

    F.Ipv4Block -> whenNonEmpty bs $
      bs & getValues 5 D.getIpv4Block <&> SIpv4Block

    F.Ipv6Block -> whenNonEmpty bs $
      bs & getValues 17 D.getIpv6Block <&> SIpv6Block

    F.Int64LE -> whenNonEmpty bs $
      bs & getValues 8 G.getInt64le <&> SInt64

    F.Int32LE -> whenNonEmpty bs $
      bs & getValues 4 G.getInt32le <&> SInt32

    F.Int16LE -> whenNonEmpty bs $
      bs & getValues 2 G.getInt16le <&> SInt16

    F.Int8 -> whenNonEmpty bs $
      bs & getValues 1 G.getInt8 <&> SInt8

    F.Word64LE -> whenNonEmpty bs $
      bs & getValues 8 G.getWord64le <&> SWord64

    F.Word32LE -> whenNonEmpty bs $
      bs & getValues 4 G.getWord32le <&> SWord32

    F.Word16LE -> whenNonEmpty bs $
      bs & getValues 2 G.getWord16le <&> SWord16

    F.Word8 -> whenNonEmpty bs $
      bs & getValues 1 G.getWord8 <&> SWord8

    F.Text ->
      [SText bs]

    F.Repeat n fmt@F.Text -> whenNonEmpty bs $
      LBS.chunkBy (fromIntegral n) bs >>= getRawValue fmt

    F.BitString ->
      [SBitString bs]

    F.Repeat n fmt@F.BitString -> whenNonEmpty bs $
      LBS.chunkBy (fromIntegral n) bs >>= getRawValue fmt

    F.Binary ->
      [SBinary bs]

    F.Repeat n fmt@F.Binary -> whenNonEmpty bs $
      LBS.chunkBy (fromIntegral n) bs >>= getRawValue fmt

    F.Bitmap ->
      [SBitmap bs]

    F.Repeat n fmt@F.Bitmap -> whenNonEmpty bs $
      LBS.chunkBy (fromIntegral n) bs >>= getRawValue fmt

    F.Repeat n fmt -> whenNonEmpty bs $
      getRawValue fmt bs & L.chunksOf (fromIntegral n) <&> SList

whenNonEmpty :: LBSC.ByteString -> [a] -> [a]
whenNonEmpty bs f =
  if LBS.null bs then [] else f

segmentValues :: Segment LBS.ByteString -> [SegmentValue]
segmentValues segment =
  case segment ^. the @"meta" . the @"format" of
    Just (Known format) -> getRawValue format (segment ^. the @"payload")
    Just (Unknown txt)  -> [SUnknown txt (segment ^. the @"payload")]
    Nothing             -> []

