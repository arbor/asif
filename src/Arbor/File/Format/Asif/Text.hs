module Arbor.File.Format.Asif.Text where

import Data.Text (Text)
import Text.Read

import qualified Data.Text as T

tShow :: Show a => a -> Text
tShow = T.pack . show

tReadMaybe :: Read a => Text -> Maybe a
tReadMaybe = readMaybe . T.unpack
