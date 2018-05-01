module App.Commands
  ( globalOptions
  ) where

import App.Commands.ExtractFiles
import App.Commands.ExtractSegments
import Data.Monoid
import Options.Applicative

globalOptions :: Parser (IO ())
globalOptions = subparser
  (   command "extract-segments"    (info commandExtractSegments    idm)
  <>  command "extract-files"       (info commandExtractFiles       idm)
  )
