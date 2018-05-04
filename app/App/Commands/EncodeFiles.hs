{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.EncodeFiles where

import App.Commands.Options.Type
import Arbor.File.Format.Asif
import Arbor.File.Format.Asif.ByteString.Builder
import Arbor.File.Format.Asif.IO
import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class                    (liftIO)
import Control.Monad.Trans.Resource              (MonadResource, runResourceT)
import Data.Function
import Data.List
import Data.Maybe
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
import qualified Data.Conduit                as C
import qualified Data.Conduit.Binary         as C
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Vector.Storable        as DVS
import qualified System.Directory            as IO
import qualified System.IO                   as IO

parseEncodeFilesOptions :: Parser EncodeFilesOptions
parseEncodeFilesOptions = EncodeFilesOptions
  <$> strOption
      (   long "source"
      <>  metavar "FILE"
      <>  help "Input file"
      )
  <*> strOption
      (   long "target"
      <>  metavar "FILE"
      <>  value "-"
      <>  help "Output file"
      )
  <*> strOption
      (   long "asif-type"
      <>  metavar "ASIF_TYPE"
      <>  help "The magic extension of the asif file"
      )

commandEncodeFiles :: Parser (IO ())
commandEncodeFiles = runResourceT . runEncodeFiles <$> parseEncodeFilesOptions

runEncodeFiles :: MonadResource m => EncodeFilesOptions -> m ()
runEncodeFiles opt = do
  let sourcePath = opt ^. L.source
  filenamesContents <- liftIO $ BS.readFile (sourcePath <> "/.asif/filenames")
  let filenames = mfilter (/= "") $ T.decodeUtf8 <$> BS.split 0 filenamesContents

  handles <- forM filenames $ \filename -> do
    h <- liftIO $ IO.openFile (sourcePath <> "/" <> T.unpack filename) IO.ReadWriteMode
    liftIO $ IO.hSeek h IO.SeekFromEnd 0
    return h

  let contents = segmentsRawC (opt ^. L.asifType) handles

  (_, hTarget) <- openFileOrStd (opt ^. L.target) IO.WriteMode

  C.runConduit $ contents .| C.sinkHandle hTarget

  liftIO $ IO.hFlush hTarget
