{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Arbor.File.Format.Asif.ByIndex
  ( ByIndex(..)
  ) where

import Data.Monoid ((<>))

import qualified Data.Semigroup as S

newtype ByIndex a = ByIndex
  { unByIndex :: [a]
  } deriving (Eq, Show)

instance (Monoid a, S.Semigroup a) => S.Semigroup (ByIndex a) where
  ByIndex as <> ByIndex bs = ByIndex (appendByIndex as bs)

instance (Monoid a, S.Semigroup a) => Monoid (ByIndex a) where
  mappend = (S.<>)
  mempty = ByIndex []

appendByIndex :: Monoid a => [a] -> [a] -> [a]
appendByIndex (a:as) (b:bs) = (a `mappend` b):appendByIndex as bs
appendByIndex (a:as) bs     =  a             :appendByIndex as bs
appendByIndex    as  (b:bs) =              b :appendByIndex as bs
appendByIndex    []     []  = []
