module App.Commands.Options.Type where

data EncodeFilesOptions = EncodeFilesOptions
  { encodeFilesOptionsSource   :: FilePath
  , encodeFilesOptionsTarget   :: FilePath
  , encodeFilesOptionsAsifType :: String
  } deriving (Eq, Show)

data ExtractFilesOptions = ExtractFilesOptions
  { extractFilesOptionsSource :: FilePath
  , extractFilesOptionsTarget :: FilePath
  } deriving (Eq, Show)

data ExtractSegmentsOptions = ExtractSegmentsOptions
  { extractSegmentsOptionsSource :: FilePath
  , extractSegmentsOptionsTarget :: FilePath
  } deriving (Eq, Show)

data DumpOptions = DumpOptions
  { dumpOptionsSource :: FilePath
  , dumpOptionsTarget :: FilePath
  } deriving (Eq, Show)
