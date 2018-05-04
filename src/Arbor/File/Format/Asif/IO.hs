module Arbor.File.Format.Asif.IO where

import Conduit
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Control.Monad.Trans.Resource
import Data.Int
import System.IO

import qualified GHC.IO.Handle as IO
import qualified System.IO     as IO

withFileOrStd :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFileOrStd filePath mode f = if filePath == "-"
  then case mode of
    ReadMode   -> f stdin
    WriteMode  -> f stdout
    AppendMode -> f stdout
    _          ->  error "Cannot open stdin or std out with read/write mode"
  else withFile filePath mode f

hGetAndResetOffset :: MonadIO m => IO.Handle -> m Int64
hGetAndResetOffset h = do
  IO.HandlePosn _ offset <- liftIO $ IO.hGetPosn h
  liftIO $ hFlush h
  liftIO $ hSeek  h AbsoluteSeek 0
  return (fromIntegral offset)

openFileOrStd :: (MonadResource m, MonadIO m) => FilePath -> IO.IOMode -> m (ReleaseKey, IO.Handle)
openFileOrStd "-" IO.WriteMode = allocate (return IO.stdout) (const (return ()))
openFileOrStd "-" IO.ReadMode = allocate (return IO.stdin) (const (return ()))
openFileOrStd filePath ioMode = allocate
  (liftIO $ IO.openFile filePath ioMode)
  (liftIO . IO.hClose)
