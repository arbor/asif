{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Dump where

import App.Commands.Options.Type
import Arbor.File.Format.Asif
import Arbor.File.Format.Asif.Data.Ip
import Arbor.File.Format.Asif.Whatever
import Control.Lens
import Control.Monad
import Data.Function
import Data.List
import Data.Monoid
import Data.Text                       (Text)
import Data.Thyme.Clock
import Data.Thyme.Clock.POSIX          (POSIXTime, getPOSIXTime)
import Data.Thyme.Format               (formatTime)
import Data.Thyme.Time.Core
import Data.Word
import Numeric                         (showHex)
import Options.Applicative
import System.Directory
import System.Locale                   (defaultTimeLocale, iso8601DateFormat)
import Text.Printf

import qualified App.Commands.Options.Lens              as L
import qualified Arbor.File.Format.Asif.ByteString.Lazy as LBS
import qualified Arbor.File.Format.Asif.Format          as F
import qualified Arbor.File.Format.Asif.Lens            as L
import qualified Data.Attoparsec.ByteString             as AP
import qualified Data.Binary                            as G
import qualified Data.Binary.Get                        as G
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.ByteString.Lazy.Char8             as LBSC
import qualified Data.Map                               as M
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import qualified Data.Vector.Storable                   as DVS
import qualified System.Directory                       as IO
import qualified System.IO                              as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

parseDumpOptions :: Parser DumpOptions
parseDumpOptions = DumpOptions
  <$> strOption
      (   long "source"
      <>  metavar "FILE"
      <>  help "Input file"
      )
  <*> strOption
      (   long "target"
      <>  metavar "FILE"
      <>  help "Output file"
      )

commandDump :: Parser (IO ())
commandDump = runDump <$> parseDumpOptions

showTime :: FormatTime t => t -> String
showTime = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S %Z"))

runDump :: DumpOptions -> IO ()
runDump opt = do
  h <- IO.openFile (opt ^. L.source) IO.ReadMode
  contents <- LBS.hGetContents h
  case extractNamedSegments magic contents of
    Left error -> do
      IO.hPutStrLn IO.stderr $ "Error occured: " <> error
      return ()
    Right namedSegments -> do
      let targetPath = opt ^. L.target

      IO.hPutStrLn IO.stderr $ "Writing to: " <> targetPath

      createDirectoryIfMissing True targetPath

      forM_ (M.toList namedSegments) $ \(path, segment) -> do
        IO.putStrLn $ "==== " <> T.unpack path <> " ===="

        case segment ^. L.meta . L.format of
          Just (Known F.StringZ) ->
            forM_ (init (LBS.split 0 (segment ^. L.payload))) $ \bs -> do
              IO.putStrLn $ T.unpack (T.decodeUtf8 (LBS.toStrict bs))
              return ()
          Just (Known (F.Repeat n F.Char)) ->
            forM_ (LBS.chunkBy (fromIntegral n) (segment ^. L.payload)) $ \bs -> do
              IO.putStrLn $ T.unpack (T.decodeUtf8 (LBS.toStrict bs))
              return ()
          Just (Known F.TimeMillis64LE) ->
            forM_ (LBS.chunkBy 8 (segment ^. L.payload)) $ \bs -> do
              let w = G.runGet G.getInt64le (LBS.take 8 (bs <> LBS.replicate 8 0))
              let t :: POSIXTime = (w `div` 1000) ^. from microseconds
              IO.putStrLn $ showTime (posixSecondsToUTCTime t) <> " (" <> show w <> " ms)"
              return ()
          Just (Known F.TimeMicros64LE) ->
            forM_ (LBS.chunkBy 8 (segment ^. L.payload)) $ \bs -> do
              let w = G.runGet G.getInt64le (LBS.take 8 (bs <> LBS.replicate 8 0))
              let t :: POSIXTime = w ^. from microseconds
              IO.putStrLn $ showTime (posixSecondsToUTCTime t) <> " (" <> show w <> " µs)"
              return ()
          Just (Known F.Ipv4) ->
            forM_ (LBS.chunkBy 4 (segment ^. L.payload)) $ \bs -> do
              let w = G.runGet G.getWord32le (LBS.take 8 (bs <> LBS.replicate 4 0))
              let ipString = w & word32ToIpv4 & ipv4ToString
              IO.putStrLn $ ipString <> replicate (16 - length ipString) ' ' <> "(" <> show w <> ")"
              return ()
          Just (Known F.Word64LE) ->
            forM_ (LBS.chunkBy 8 (segment ^. L.payload)) $ \bs -> do
              let w = G.runGet G.getWord64le (LBS.take 8 (bs <> LBS.replicate 8 0))
              IO.print w
              return ()
          _ ->
            forM_ (LBS.chunkBy 16 (segment ^. L.payload)) $ \bs -> do
              IO.putStrLn $ mconcat (intersperse " " (reverse . take 2 . reverse . ('0':) . flip showHex "" <$> LBS.unpack bs))
              return ()

  where magic = AP.string "seg:" *> (BS.pack <$> many AP.anyWord8) AP.<?> "\"seg:????\""
