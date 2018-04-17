module App.Commands
  ( globalOptions
  ) where

import App.Commands.ExtractSegments
import Data.Monoid
import Options.Applicative

globalOptions :: Parser (IO ())
globalOptions = subparser
  (   command "extract-segments"    (info commandExtractSegments    idm)
  )
