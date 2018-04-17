module App.IO where

import System.IO

withFileOrStd :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFileOrStd filePath mode f = if filePath == "-"
  then case mode of
    ReadMode   -> f stdin
    WriteMode  -> f stdout
    AppendMode -> f stdout
    _          ->  error "Cannot open stdin or std out with read/write mode"
  else withFile filePath mode f
