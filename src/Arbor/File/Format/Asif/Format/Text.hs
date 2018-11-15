{-# LANGUAGE LambdaCase #-}
module Arbor.File.Format.Asif.Format.Text
( rawValueToText
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

rawValueToText :: SegmentValue -> Text
rawValueToText = \case
  SString v -> T.decodeUtf8 (LBS.toStrict v)
  SChar v -> T.singleton v
  STime v -> T.pack $ show v

  SIpv4 v -> T.pack $ ipv4ToString v
  SIpv6 v ->
    case ipv6ToWord32x4 v of
      (a, b, c, d) | a == 0 && b == 0 && c == 0xFFFF -> d & word32ToIpv4 & ipv4ToString & T.pack
      _ -> T.pack $ ipv6ToString v

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

  SUnknown _ v -> LBS.chunkBy 4 v <&> toHex & L.intersperse " " & mconcat


toHex :: LBS.ByteString -> Text
toHex bs =
  T.pack $ mconcat (L.reverse . L.take 2 . L.reverse . ('0':) . flip showHex "" <$> LBS.unpack bs)
