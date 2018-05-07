{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Arbor.File.Format.Asif.ByIndex
  ( ByIndex(..)
  ) where

import Arbor.File.Format.Asif.ByteString.Builder
import Arbor.File.Format.Asif.Format             (Format)
import Arbor.File.Format.Asif.Get
import Arbor.File.Format.Asif.Search
import Arbor.File.Format.Asif.Text
import Arbor.File.Format.Asif.Type
import Arbor.File.Format.Asif.Whatever
import Control.Lens
import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Builder
import Data.Either
import Data.Either.Combinators
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text                                 (Text)
import Data.Text.Encoding                        (decodeUtf8')
import Data.Text.Encoding.Error
import Data.Thyme.Clock                          (microseconds)
import Data.Thyme.Clock.POSIX                    (POSIXTime)
import Data.Traversable
import Data.Word

import qualified Arbor.File.Format.Asif.Format as F
import qualified Arbor.File.Format.Asif.Lens   as L
import qualified Data.Attoparsec.ByteString    as AP
import qualified Data.Binary.Get               as G
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LC8
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as LT
import qualified Data.Text.Lazy                as T
import qualified Data.Vector.Unboxed           as VU

newtype ByIndex a = ByIndex
  { unByIndex :: [a]
  } deriving (Eq, Show)

instance Monoid a => Monoid (ByIndex a) where
  mappend (ByIndex as) (ByIndex bs) = ByIndex (go as bs)
    where go (a:as) (b:bs) = (a <> b):go as bs
          go (a:as) bs     =  a      :go as bs
          go    as  (b:bs) =       b :go as bs
          go    []     []  = []
  mempty = ByIndex []
