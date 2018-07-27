{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.ExtractFiles where

import App.Commands.Options.Type
import Arbor.File.Format.Asif.IO
import Arbor.File.Format.Asif.Segment
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource   (MonadResource, runResourceT)
import Data.Generics.Product.Any
import Data.List
import Data.Maybe
import Data.Monoid
import Options.Applicative
import System.Directory

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified System.Directory           as IO
import qualified System.IO                  as IO

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
  (_, hIn) <- openFileOrStd (opt ^. the @"source") IO.ReadMode
  contents <- liftIO $ LBS.hGetContents hIn
  case extractSegments magic contents of
    Left errorMessage -> do
      liftIO $ IO.hPutStrLn IO.stderr $ "Error occured: " <> errorMessage
      return ()
    Right segments -> do
      let filenames = fromMaybe "" . (^. the @"meta" . the @"filename") <$> segments
      let namedSegments = M.fromList $ mfilter ((/= "") . fst) (zip filenames segments)
      let targetPath = opt ^. the @"target"

      liftIO $ IO.hPutStrLn IO.stderr $ "Writing to: " <> targetPath
      liftIO $ createDirectoryIfMissing True targetPath

      forM_ (zip [0..] filenames) $ \(i :: Int, filename) ->
        case M.lookup filename namedSegments of
          Just segment -> do
            let outFilename = T.pack targetPath <> "/" <> filename
            let basename = mconcat (intersperse "/" (init (T.splitOn "/" outFilename)))
            liftIO $ IO.createDirectoryIfMissing True (T.unpack basename)
            liftIO $ LBS.writeFile (T.unpack outFilename) (segment ^. the @"payload")
          Nothing ->
            liftIO $ IO.hPutStrLn IO.stderr $ "Segment " <> show i <> " has no filename.  Skipping"

  where magic = AP.string "seg:" *> (BS.pack <$> many AP.anyWord8) AP.<?> "\"seg:????\""
