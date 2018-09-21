module Main
  ( main
  ) where

import App.Commands
import Control.Monad
import Data.Semigroup      ((<>))
import Options.Applicative

import qualified System.IO as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stderr IO.LineBuffering
  join $ customExecParser
    (prefs $ showHelpOnEmpty <> showHelpOnError)
    (info (globalOptions <**> helper) idm)
