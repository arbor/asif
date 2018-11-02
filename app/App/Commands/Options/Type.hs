{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Type where

import GHC.Generics

data EncodeFilesOptions = EncodeFilesOptions
  { source   :: FilePath
  , target   :: FilePath
  , asifType :: String
  } deriving (Eq, Show, Generic)

data ExtractFilesOptions = ExtractFilesOptions
  { source :: FilePath
  , target :: FilePath
  } deriving (Eq, Show, Generic)

data ExtractSegmentsOptions = ExtractSegmentsOptions
  { source :: FilePath
  , target :: FilePath
  } deriving (Eq, Show, Generic)

data DumpOptions = DumpOptions
  { source :: FilePath
  , target :: FilePath
  } deriving (Eq, Show, Generic)

data DumpOnlyOptions = DumpOnlyOptions
  { source    :: FilePath
  , target    :: FilePath
  , segments  :: [Int]
  , filenames :: [FilePath]
  } deriving (Eq, Show, Generic)

data LsOptions = LsOptions
  { source :: FilePath
  , target :: FilePath
  } deriving (Eq, Show, Generic)
