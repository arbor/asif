module Main
  ( main
  ) where

import App.Commands
import Control.Monad
import Data.Semigroup
import Options.Applicative

main :: IO ()
main = join $ customExecParser
  (prefs $ showHelpOnEmpty <> showHelpOnError)
  (info (globalOptions <**> helper) idm)
