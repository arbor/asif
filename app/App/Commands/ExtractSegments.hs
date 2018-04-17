{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.ExtractSegments where

import App.Commands.ExtractSegments.Type
import Arbor.File.Format.Asif
import Control.Lens
import Control.Monad
import Data.Function
import Data.Monoid
import Data.Word
-- import HaskellWorks.Data.Bits.BitWise
import Options.Applicative
import System.Directory
import Text.Printf

import qualified App.Commands.ExtractSegments.Lens as L
import qualified Data.Attoparsec.ByteString        as AP
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.ByteString.Lazy.Char8        as LBSC
import qualified Data.Vector.Storable              as DVS
import qualified System.IO                         as IO

parseCommandOptions :: Parser CommandOptions
parseCommandOptions = CommandOptions
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

commandExtractSegments :: Parser (IO ())
commandExtractSegments = runExtractSegments <$> parseCommandOptions

runExtractSegments :: CommandOptions -> IO ()
runExtractSegments opt = do
  h <- IO.openFile (opt ^. L.source) IO.ReadMode
  contents <- LBS.hGetContents h
  -- TODO pass in magic
  case extractSegments magic contents of
    Left error -> do
      IO.hPutStrLn IO.stderr $ "Error occured: " <> error
      return ()
    Right segments -> do
      let targetPath = opt ^. L.target

      IO.hPutStrLn IO.stderr $ "Writing to: " <> targetPath

      createDirectoryIfMissing True targetPath

      forM_ (zip [0..] segments) $ \(i :: Int, segment) ->
        LBS.writeFile (targetPath <> "/" <> printf "%03d" i <> ".seg") segment
  where magic = AP.string "seg:" *> (BS.pack <$> many AP.anyWord8) AP.<?> "\"seg:????\""
