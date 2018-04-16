module App.Commands.ExtractSegments.Type where

data CommandOptions = CommandOptions
  { commandOptionsSource :: FilePath
  , commandOptionsTarget :: FilePath
  } deriving (Eq, Show)
