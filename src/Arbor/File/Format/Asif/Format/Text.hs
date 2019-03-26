{-# LANGUAGE LambdaCase #-}
module Arbor.File.Format.Asif.Format.Text
( segmentValueToText
)
where

import qualified Arbor.File.Format.Asif.ByteString.Lazy     as LBS
import           Arbor.File.Format.Asif.Data.Ip
import           Arbor.File.Format.Asif.Format.SegmentValue
import           Control.Lens
import qualified Data.ByteString.Lazy                       as LBS
import qualified Data.List                                  as L
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import qualified Data.Text.Encoding                         as T
import           HaskellWorks.Data.Bits.BitShow             (bitShow)
import           Numeric                                    (showHex)

segmentValueToText :: SegmentValue -> Text
segmentValueToText = \case
  SString v -> T.decodeUtf8 (LBS.toStrict v)
  SBool v -> T.pack $ show v
  SChar v -> T.singleton v
  STime v -> T.pack $ show v

  SIpv4 v -> T.pack $ ipv4ToString v
  SIpv6 v -> T.pack $ ipv6toStringCollapseV4 v
  SIpv4Block v -> T.pack $ ipv4CidrToString v
  SIpv6Block v -> T.pack $ ipv6CidrToStringCollapseV4 v

  SInt64 v -> T.pack $ show v
  SInt32 v -> T.pack $ show v
  SInt16 v -> T.pack $ show v
  SInt8  v -> T.pack $ show v

  SWord64 v -> T.pack $ show v
  SWord32 v -> T.pack $ show v
  SWord16 v -> T.pack $ show v
  SWord8  v -> T.pack $ show v

  SText v -> T.decodeUtf8 (LBS.toStrict v)

  SBitString v  -> T.pack $ bitShow v
  SBitmap v     -> T.pack $ bitShow v
  SBinary v     -> LBS.chunkBy 4 v <&> toHex & L.intersperse " " & mconcat

  SList vs -> vs <&> segmentValueToText & T.intercalate ","

  SUnknown _ v -> LBS.chunkBy 4 v <&> toHex & L.intersperse " " & mconcat

toHex :: LBS.ByteString -> Text
toHex bs =
  T.pack $ mconcat (L.reverse . L.take 2 . L.reverse . ('0':) . flip showHex "" <$> LBS.unpack bs)
