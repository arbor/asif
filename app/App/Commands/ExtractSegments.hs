{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.ExtractSegments where

import App.Commands.Options.Type
import Arbor.File.Format.Asif
import Arbor.File.Format.Asif.IO
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.Function
import Data.Monoid
import Data.Word
import Options.Applicative
import System.Directory
import Text.Printf

import qualified App.Commands.Options.Lens   as L
import qualified Arbor.File.Format.Asif.Lens as L
import qualified Data.Attoparsec.ByteString  as AP
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LBSC
import qualified Data.Vector.Storable        as DVS
import qualified System.IO                   as IO

parseExtractSegmentsOptions :: Parser ExtractSegmentsOptions
parseExtractSegmentsOptions = ExtractSegmentsOptions
  <$> strOption
      (   long "source"
      <>  metavar "FILE"
      <>  value "-"
      <>  help "Input file"
      )
  <*> strOption
      (   long "target"
      <>  metavar "PATH"
      <>  help "Output directory"
      )

commandExtractSegments :: Parser (IO ())
commandExtractSegments = runResourceT . runExtractSegments <$> parseExtractSegmentsOptions

runExtractSegments :: MonadResource m => ExtractSegmentsOptions -> m ()
runExtractSegments opt = do
  (_, hIn) <- openFileOrStd (opt ^. L.source) IO.ReadMode
  contents <- liftIO $ LBS.hGetContents hIn
  -- TODO pass in magic
  case extractSegments magic contents of
    Left error -> do
      liftIO $ IO.hPutStrLn IO.stderr $ "Error occured: " <> error
      return ()
    Right segments -> do
      let targetPath = opt ^. L.target

      liftIO $ IO.hPutStrLn IO.stderr $ "Writing to: " <> targetPath

      liftIO $ createDirectoryIfMissing True targetPath

      forM_ (zip [0..] segments) $ \(i :: Int, segment) ->
        liftIO $ LBS.writeFile (targetPath <> "/" <> printf "%03d" i <> ".seg") (segment ^. L.payload)
  where magic = AP.string "seg:" *> (BS.pack <$> many AP.anyWord8) AP.<?> "\"seg:????\""
