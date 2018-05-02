module Arbor.File.Format.Asif.Whatever where

import Control.Applicative
import Data.Either
import Data.Text                       (Text)
import GHC.Base
import GHC.Read
import Text.ParserCombinators.ReadP    as P
import Text.ParserCombinators.ReadPrec

import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Text.ParserCombinators.ReadP    as P
import qualified Text.ParserCombinators.ReadPrec as R

data Whatever a = Known a | Unknown Text deriving Eq

instance Show a => Show (Whatever a) where
  showsPrec n (Known a)   = showsPrec n a
  showsPrec n (Unknown t) = (T.unpack t ++)

showWhatever :: Show a => Whatever a -> String
showWhatever (Known a)   = show a
showWhatever (Unknown a) = T.unpack a

readWhatever :: Read a => String -> Whatever a
readWhatever s =
  case [x | (x,"") <- R.readPrec_to_S read' R.minPrec s] of
    [x] -> Known x
    _   -> Unknown (T.pack s)
  where read' = do
                  x <- readPrec
                  lift P.skipSpaces
                  return x

tShowWhatever :: Show a => Whatever a -> Text
tShowWhatever = T.pack . showWhatever

tReadWhatever :: Read a => Text -> Whatever a
tReadWhatever = readWhatever . T.unpack
