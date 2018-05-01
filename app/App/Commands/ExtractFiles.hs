{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.ExtractFiles where

import App.Commands.Options.Type
import Arbor.File.Format.Asif
import Control.Lens
import Control.Monad
import Data.Function
import Data.List
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
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Data.Vector.Storable        as DVS
import qualified System.Directory            as IO
import qualified System.IO                   as IO

parseExtractFilesOptions :: Parser ExtractFilesOptions
parseExtractFilesOptions = ExtractFilesOptions
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

commandExtractFiles :: Parser (IO ())
commandExtractFiles = runExtractFiles <$> parseExtractFilesOptions

runExtractFiles :: ExtractFilesOptions -> IO ()
runExtractFiles opt = do
  h <- IO.openFile (opt ^. L.source) IO.ReadMode
  contents <- LBS.hGetContents h
  -- TODO pass in magic
  case extractNamedSegments magic contents of
    Left error -> do
      IO.hPutStrLn IO.stderr $ "Error occured: " <> error
      return ()
    Right namedSegments -> do
      let targetPath = opt ^. L.target

      IO.hPutStrLn IO.stderr $ "Writing to: " <> targetPath

      createDirectoryIfMissing True targetPath

      forM_ (M.toList namedSegments) $ \(path, segment) -> do
        let filename = T.pack targetPath <> "/" <> path
        let basename = mconcat (intersperse "/" (init (T.splitOn "/" filename)))
        IO.createDirectoryIfMissing True (T.unpack basename)
        LBS.writeFile (T.unpack filename) (segment ^. L.payload)

  where magic = AP.string "seg:" *> (BS.pack <$> many AP.anyWord8) AP.<?> "\"seg:????\""