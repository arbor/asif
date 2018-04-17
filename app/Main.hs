module Main
  ( main
  ) where

import App.Commands
import Control.Monad
import Options.Applicative

main :: IO ()
main = join $ execParser (info (globalOptions <**> helper) idm)
