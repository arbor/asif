{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.DumpBitmap
  ( commandDumpBitmap
  ) where

import App.Commands.Options.Type
import App.Dump
import Arbor.File.Format.Asif.Data.Ip
import Arbor.File.Format.Asif.IO
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Trans.Resource   (MonadResource, runResourceT)
import Data.Generics.Product.Any
import Data.Monoid                    ((<>))
import Options.Applicative

import qualified Arbor.File.Format.Asif.ByteString.Lazy as LBS
import qualified Data.Binary.Get                        as G
import qualified Data.ByteString.Lazy                   as LBS
import qualified System.IO                              as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

parseDumpBitmapOptions :: Parser DumpBitmapOptions
parseDumpBitmapOptions = DumpBitmapOptions
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

commandDumpBitmap :: Parser (IO ())
commandDumpBitmap = runResourceT . runDump <$> parseDumpBitmapOptions

runDump :: MonadResource m => DumpBitmapOptions -> m ()
runDump opt = do
  (_, hIn)  <- openFileOrStd (opt ^. the @"source") IO.ReadMode
  (_, hOut) <- openFileOrStd (opt ^. the @"target") IO.WriteMode

  contents <- liftIO $ LBS.hGetContents hIn

  forM_ (zip [0..] (G.runGet G.getWord64le <$> LBS.chunkBy 8 contents)) $ \(idx, w64) ->
    forM_ (word64ToList idx w64 []) $ \w32 -> do
      let ipString = w32 & word32ToIpv4 & ipv4ToString
      liftIO $ IO.hPutStrLn hOut $ ipString <> replicate (16 - length ipString) ' ' <> "(" <> show w32 <> ")"
