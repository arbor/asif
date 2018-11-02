{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Dump where

import App.Commands.Options.Type
import App.Dump
import Arbor.File.Format.Asif.IO
import Arbor.File.Format.Asif.Segment
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Trans.Resource   (MonadResource, runResourceT)
import Data.Generics.Product.Any
import Data.Maybe
import Data.Monoid                    ((<>))
import Options.Applicative

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified System.IO                  as IO

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
        dumpSegment hOut i filename segment

  where magic = AP.string "seg:" *> (BS.pack <$> many AP.anyWord8) AP.<?> "\"seg:????\""
