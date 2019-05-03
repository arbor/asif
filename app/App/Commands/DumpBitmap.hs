{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.DumpBitmap
  ( commandDumpBitmap
  ) where

import App.Commands.Options.Type
import Arbor.File.Format.Asif.Data.Ip
import Arbor.File.Format.Asif.Extract (bitmap)
import Arbor.File.Format.Asif.IO
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Trans.Resource   (MonadResource, runResourceT)
import Data.Generics.Product.Any
import Data.Monoid                    ((<>))
import Options.Applicative

import qualified Data.ByteString.Lazy as LBS
import qualified System.IO            as IO

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

  forM_ (bitmap contents) $ \ip ->
    let ipString = ipv4ToString ip
        w32 = ipv4ToWord32 ip
    in liftIO $ IO.hPutStrLn hOut $ ipString <> replicate (16 - length ipString) ' ' <> "(" <> show w32 <> ")"
