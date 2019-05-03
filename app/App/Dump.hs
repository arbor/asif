{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Dump
  ( dumpSegment
  ) where

import Arbor.File.Format.Asif.Data.Ip
import Arbor.File.Format.Asif.Extract
import Arbor.File.Format.Asif.Segment
import Arbor.File.Format.Asif.Whatever
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class          (liftIO)
import Control.Monad.Trans.Resource    (MonadResource)
import Data.Char                       (isPrint)
import Data.Generics.Product.Any
import Data.List
import Data.Monoid                     ((<>))
import Data.Text                       (Text)
import Data.Thyme.Format               (formatTime)
import Data.Thyme.Time.Core
import HaskellWorks.Data.Bits.BitShow
import Numeric                         (showHex)
import System.IO                       (Handle)
import System.Locale                   (defaultTimeLocale, iso8601DateFormat)

import qualified Arbor.File.Format.Asif.ByteString.Lazy as LBS
import qualified Arbor.File.Format.Asif.Format          as F
import qualified Arbor.File.Format.Asif.Get             as D
import qualified Data.Binary                            as G
import qualified Data.Binary.Get                        as G
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.ByteString.Lazy.Char8             as LBSC
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import qualified HaskellWorks.Data.Network.Ip.Ipv4      as IP4
import qualified HaskellWorks.Data.Network.Ip.Ipv6      as IP6
import qualified System.IO                              as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

showTime :: FormatTime t => t -> String
showTime = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S %Z"))


dumpSegment :: MonadResource m => Handle -> Int -> Text -> Segment LBS.ByteString -> m ()
dumpSegment hOut i filename segment = do
  if T.null filename
    then liftIO $ IO.hPutStrLn hOut $ "==== [" <> show i <> "] ===="
    else liftIO $ IO.hPutStrLn hOut $ "==== [" <> show i <> "] " <> T.unpack filename <> " ===="

  case segment ^. the @"meta" . the @"format" of
    Just (Known F.StringZ) -> do
      when (LBS.length (segment ^. the @"payload") > 0) $
        forM_ (init (LBS.split 0 (segment ^. the @"payload"))) $ \bs ->
          liftIO $ IO.hPutStrLn hOut $ T.unpack (T.decodeUtf8 (LBS.toStrict bs))

    Just (Known (F.Repeat n F.Char)) ->
      forM_ (LBS.chunkBy (fromIntegral n) (segment ^. the @"payload")) $ \bs ->
        liftIO $ IO.hPutStrLn hOut $ T.unpack (T.decodeUtf8 (LBS.toStrict bs))

    Just (Known F.TimeMillis64LE) ->
      forM_ (LBS.chunkBy 8 (segment ^. the @"payload")) $ \bs -> do
        let t = G.runGet D.getTimeMillis (LBS.take 8 (bs <> LBS.replicate 8 0))
        let ms = t & utcTimeToPOSIXSeconds & toMicroseconds & (`div` 1000)
        liftIO $ IO.hPutStrLn hOut $ showTime t <> " (" <> show ms <> " ms)"

    Just (Known F.TimeMicros64LE) ->
      forM_ (LBS.chunkBy 8 (segment ^. the @"payload")) $ \bs -> do
        let t = G.runGet D.getTimeMicros (LBS.take 8 (bs <> LBS.replicate 8 0))
        let micros = t & utcTimeToPOSIXSeconds & toMicroseconds
        liftIO $ IO.hPutStrLn hOut $ showTime t <> " (" <> show micros <> " µs)"

    Just (Known F.Ipv4) ->
      forM_ (LBS.chunkBy 4 (segment ^. the @"payload")) $ \bs -> do
        let w = G.runGet D.getIpv4 (LBS.take 4 (bs <> LBS.replicate 4 0))
        let ipString = w & ipv4ToString
        liftIO $ IO.hPutStrLn hOut $ ipString <> replicate (16 - length ipString) ' ' <> "(" <> show w <> ")"

    Just (Known F.Ipv6) ->
      forM_ (LBS.chunkBy 16 (segment ^. the @"payload")) $ \bs -> do
        let ip6@(IP6.IpAddress (a,b,c,d)) = G.runGet D.getIpv6 (LBS.take 16 (bs <> LBS.replicate 16 0))
        let ipString = ipv6toStringCollapseV4 ip6
        let raw = case isIpv4 ip6 of
              Just (IP4.IpAddress w32) -> "(" <> show w32 <> ")"
              Nothing                  -> "(" <> show (a, b, c, d) <> ")"
        liftIO $ IO.hPutStrLn hOut $ ipString <> replicate (64 - length ipString) ' ' <> raw

    Just (Known F.Ipv4Block) ->
      forM_ (LBS.chunkBy 5 (segment ^. the @"payload")) $ \bs -> do
        let block@(IP4.IpBlock ip mask) = G.runGet D.getIpv4Block (LBS.take 5 (bs <> LBS.replicate 5 0))
        let IP4.IpAddress ipw = ip
        let IP4.IpNetMask maskw = mask
        let ipString = block & ipv4CidrToString
        liftIO $ IO.hPutStrLn hOut $ ipString <> replicate (64 - length ipString) ' ' <> "(" <> show (ipw, maskw) <> ")"

    Just (Known F.Ipv6Block) ->
      forM_ (LBS.chunkBy 17 (segment ^. the @"payload")) $ \bs -> do
        let block@(IP6.IpBlock ip mask) = G.runGet D.getIpv6Block (LBS.take 17 (bs <> LBS.replicate 17 0))
        let IP6.IpAddress ipw = ip
        let IP6.IpNetMask maskw = mask
        let ipString = block & ipv6CidrToStringCollapseV4
        liftIO $ IO.hPutStrLn hOut $ ipString <> replicate (64 - length ipString) ' ' <> "(" <> show (ipw, maskw) <> ")"

    Just (Known F.Int64LE) ->
      forM_ (LBS.chunkBy 8 (segment ^. the @"payload")) $ \bs -> do
        let w = G.runGet G.getInt64le (LBS.take 8 (bs <> LBS.replicate 8 0))
        liftIO $ IO.hPrint hOut w

    Just (Known F.Int32LE) ->
      forM_ (LBS.chunkBy 4 (segment ^. the @"payload")) $ \bs -> do
        let w = G.runGet G.getInt32le (LBS.take 4 (bs <> LBS.replicate 4 0))
        liftIO $ IO.hPrint hOut w

    Just (Known F.Int16LE) ->
      forM_ (LBS.chunkBy 2 (segment ^. the @"payload")) $ \bs -> do
        let w = G.runGet G.getInt16le (LBS.take 2 (bs <> LBS.replicate 2 0))
        liftIO $ IO.hPrint hOut w

    Just (Known F.Int8) ->
      forM_ (LBS.chunkBy 1 (segment ^. the @"payload")) $ \bs -> do
        let w = G.runGet G.getInt8 (LBS.take 1 (bs <> LBS.replicate 1 0))
        liftIO $ IO.hPrint hOut w

    Just (Known F.Word64LE) ->
      forM_ (LBS.chunkBy 8 (segment ^. the @"payload")) $ \bs -> do
        let w = G.runGet G.getWord64le (LBS.take 8 (bs <> LBS.replicate 8 0))
        liftIO $ IO.hPrint hOut w

    Just (Known F.Word32LE) ->
      forM_ (LBS.chunkBy 4 (segment ^. the @"payload")) $ \bs -> do
        let w = G.runGet G.getWord32le (LBS.take 4 (bs <> LBS.replicate 4 0))
        liftIO $ IO.hPrint hOut w

    Just (Known F.Word16LE) ->
      forM_ (LBS.chunkBy 2 (segment ^. the @"payload")) $ \bs -> do
        let w = G.runGet G.getWord16le (LBS.take 2 (bs <> LBS.replicate 2 0))
        liftIO $ IO.hPrint hOut w

    Just (Known F.Word8) ->
      forM_ (LBS.chunkBy 1 (segment ^. the @"payload")) $ \bs -> do
        let w = G.runGet G.getWord8 (LBS.take 1 (bs <> LBS.replicate 1 0))
        liftIO $ IO.hPrint hOut w

    Just (Known F.Text) ->
      liftIO $ LBSC.hPutStrLn hOut (segment ^. the @"payload")

    Just (Known F.BitString) ->
      liftIO $ IO.hPutStrLn hOut (bitShow (segment ^. the @"payload"))

    Just (Known F.Bitmap) ->
      forM_ (bitmap $ segment ^. the @"payload") $ \ip ->
        let ipString = ipv4ToString ip
            w32 = ipv4ToWord32 ip
        in liftIO $ IO.hPutStrLn hOut $ ipString <> replicate (16 - length ipString) ' ' <> "(" <> show w32 <> ")"

    Just (Known F.Bool) ->
      forM_ (LBS.chunkBy 1 (segment ^. the @"payload")) $ \bs -> do
        let w :: Bool = G.runGet G.get (LBS.take 1 (bs <> LBS.replicate 1 0))
        liftIO $ IO.hPrint hOut w

    -- WARNING: Don't put a true catch-all (_ -> ...) case here
    -- it means we miss this function when new formats get added.
    Just (Known F.Binary) -> catchAll
    Just (Known F.Char) -> catchAll
    Just (Known (F.Repeat _ _)) -> catchAll
    Just (Unknown _) -> catchAll
    Nothing -> catchAll
  where
    catchAll = forM_ (zip (LBS.chunkBy 16 (segment ^. the @"payload")) [0 :: Int, 16..]) $ \(bs, j) -> do
      let bytes = mconcat (intersperse " " (reverse . take 2 . reverse . ('0':) . flip showHex "" <$> LBS.unpack bs))
      liftIO $ IO.hPutStr hOut $ reverse $ take 8 $ reverse $ ("0000000" ++) $ showHex j ""
      liftIO $ IO.hPutStr hOut "  "
      liftIO $ IO.hPutStr hOut $ bytes <> replicate (47 - length bytes) ' '
      liftIO $ IO.hPutStr hOut "  "
      liftIO $ IO.hPutStr hOut $ (\c -> if isPrint c then c else '.') <$> LBSC.unpack bs
      liftIO $ IO.hPutStrLn hOut ""
