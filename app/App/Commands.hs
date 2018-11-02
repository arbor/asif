module App.Commands
  ( globalOptions
  ) where

import App.Commands.Dump
import App.Commands.DumpOnly
import App.Commands.EncodeFiles
import App.Commands.ExtractFiles
import App.Commands.ExtractSegments
import App.Commands.Ls
import Data.Monoid                  ((<>))
import Options.Applicative

globalOptions :: Parser (IO ())
globalOptions = subparser
  (   command "dump"                (info commandDump               idm)
  <>  command "dump-only"           (info commandDumpOnly           idm)
  <>  command "encode-files"        (info commandEncodeFiles        idm)
  <>  command "extract-files"       (info commandExtractFiles       idm)
  <>  command "extract-segments"    (info commandExtractSegments    idm)
  <>  command "ls"                  (info commandLs                 idm)
  )
