{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Dump where

import App.Commands.Options.Type
import Arbor.File.Format.Asif.Data.Ip
import Arbor.File.Format.Asif.IO
import Arbor.File.Format.Asif.Segment
import Arbor.File.Format.Asif.Whatever
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class          (liftIO)
import Control.Monad.Trans.Resource    (MonadResource, runResourceT)
import Data.Char                       (isPrint)
import Data.Generics.Product.Any
import Data.List
import Data.Maybe
import Data.Monoid                     ((<>))
import Data.Thyme.Clock
import Data.Thyme.Clock.POSIX          (POSIXTime)
import Data.Thyme.Format               (formatTime)
import Data.Thyme.Time.Core
import HaskellWorks.Data.Bits.BitShow
import Numeric                         (showHex)
import Options.Applicative
import System.Locale                   (defaultTimeLocale, iso8601DateFormat)

import qualified Arbor.File.Format.Asif.ByteString.Lazy as LBS
import qualified Arbor.File.Format.Asif.Format          as F
import qualified Data.Attoparsec.ByteString             as AP
import qualified Data.Binary                            as G
import qualified Data.Binary.Get                        as G
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.ByteString.Lazy.Char8             as LBSC
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import qualified System.IO                              as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

parseDumpOptions :: Parser DumpOptions
parseDumpOptions = DumpOptions
  <$> strOption
      (   long "source"
      <>  metavar "FILE"
      <>  value "-"
      <>  help "Input file"
      )
  <*> strOption
      (   long "target"
      <>  metavar "FILE"
      <>  value "-"
      <>  help "Output file"
      )

commandDump :: Parser (IO ())
commandDump = runResourceT . runDump <$> parseDumpOptions

showTime :: FormatTime t => t -> String
showTime = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S %Z"))

runDump :: MonadResource m => DumpOptions -> m ()
runDump opt = do
  (_, hIn)  <- openFileOrStd (opt ^. the @"source") IO.ReadMode
  (_, hOut) <- openFileOrStd (opt ^. the @"target") IO.WriteMode

  contents <- liftIO $ LBS.hGetContents hIn

  case extractSegments magic contents of
    Left errorMessage -> do
      liftIO $ IO.hPutStrLn IO.stderr $ "Error occured: " <> errorMessage
      return ()
    Right segments -> do
      let filenames = fromMaybe "" . (^. the @"meta" . the @"filename") <$> segments
      let namedSegments = zip filenames segments

      forM_ (zip [0..] namedSegments) $ \(i :: Int, (filename, segment)) -> do
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
              let w = G.runGet G.getInt64le (LBS.take 8 (bs <> LBS.replicate 8 0))
              let t :: POSIXTime = (w * 1000) ^. from microseconds
              liftIO $ IO.hPutStrLn hOut $ showTime (posixSecondsToUTCTime t) <> " (" <> show w <> " ms)"
          Just (Known F.TimeMicros64LE) ->
            forM_ (LBS.chunkBy 8 (segment ^. the @"payload")) $ \bs -> do
              let w = G.runGet G.getInt64le (LBS.take 8 (bs <> LBS.replicate 8 0))
              let t :: POSIXTime = w ^. from microseconds
              liftIO $ IO.hPutStrLn hOut $ showTime (posixSecondsToUTCTime t) <> " (" <> show w <> " µs)"
          Just (Known F.Ipv4) ->
            forM_ (LBS.chunkBy 4 (segment ^. the @"payload")) $ \bs -> do
              let w = G.runGet G.getWord32le (LBS.take 8 (bs <> LBS.replicate 4 0))
              let ipString = w & word32ToIpv4 & ipv4ToString
              liftIO $ IO.hPutStrLn hOut $ ipString <> replicate (16 - length ipString) ' ' <> "(" <> show w <> ")"
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
          _ ->
            forM_ (zip (LBS.chunkBy 16 (segment ^. the @"payload")) [0 :: Int, 16..]) $ \(bs, j) -> do
              let bytes = mconcat (intersperse " " (reverse . take 2 . reverse . ('0':) . flip showHex "" <$> LBS.unpack bs))
              liftIO $ IO.hPutStr hOut $ reverse $ take 8 $ reverse $ ("0000000" ++) $ showHex j ""
              liftIO $ IO.hPutStr hOut "  "
              liftIO $ IO.hPutStr hOut $ bytes <> replicate (47 - length bytes) ' '
              liftIO $ IO.hPutStr hOut "  "
              liftIO $ IO.hPutStr hOut $ (\c -> if isPrint c then c else '.') <$> LBSC.unpack bs
              liftIO $ IO.hPutStrLn hOut ""

  where magic = AP.string "seg:" *> (BS.pack <$> many AP.anyWord8) AP.<?> "\"seg:????\""
