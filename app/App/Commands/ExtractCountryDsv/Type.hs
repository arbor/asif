module App.Commands.ExtractCountryDsv.Type where

data CommandOptions = CommandOptions
  { commandOptionsSource :: FilePath
  , commandOptionsTarget :: Maybe FilePath
  } deriving (Eq, Show)
