module Arbor.File.Format.Asif.Data.Read
  ( stringToAnyDigits
  ) where

import Control.Monad
import Text.Read

stringToAnyDigits :: (Read a, Show a) => String -> Maybe a
stringToAnyDigits str = mfilter ((== str) . show) $ readMaybe str
