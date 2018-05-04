{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.ExtractFiles where

import App.Commands.Options.Type
import Arbor.File.Format.Asif
import Arbor.File.Format.Asif.IO
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
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
      <>  value "-"
      <>  help "Input file"
      )
  <*> strOption
      (   long "target"
      <>  metavar "PATH"
      <>  help "Output directory"
      )

commandExtractFiles :: Parser (IO ())
commandExtractFiles = runResourceT . runExtractFiles <$> parseExtractFilesOptions

runExtractFiles :: MonadResource m => ExtractFilesOptions -> m ()
runExtractFiles opt = do
  (_, hIn) <- openFileOrStd (opt ^. L.source) IO.ReadMode
  contents <- liftIO $ LBS.hGetContents hIn
  case extractNamedSegments magic contents of
    Left error -> do
      liftIO $ IO.hPutStrLn IO.stderr $ "Error occured: " <> error
      return ()
    Right namedSegments -> do
      let targetPath = opt ^. L.target

      liftIO $ IO.hPutStrLn IO.stderr $ "Writing to: " <> targetPath
      liftIO $ createDirectoryIfMissing True targetPath

      forM_ (M.toList namedSegments) $ \(path, segment) -> do
        let filename = T.pack targetPath <> "/" <> path
        let basename = mconcat (intersperse "/" (init (T.splitOn "/" filename)))
        liftIO $ IO.createDirectoryIfMissing True (T.unpack basename)
        liftIO $ LBS.writeFile (T.unpack filename) (segment ^. L.payload)

  where magic = AP.string "seg:" *> (BS.pack <$> many AP.anyWord8) AP.<?> "\"seg:????\""
