{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Arbor.File.Format.Asif.ByIndex
  ( ByIndex(..)
  ) where

import Data.Monoid

newtype ByIndex a = ByIndex
  { unByIndex :: [a]
  } deriving (Eq, Show)

instance Monoid a => Monoid (ByIndex a) where
  mappend (ByIndex as) (ByIndex bs) = ByIndex (appendByIndex as bs)
  mempty = ByIndex []

appendByIndex :: Monoid a => [a] -> [a] -> [a]
appendByIndex (a:as) (b:bs) = (a <> b):appendByIndex as bs
appendByIndex (a:as) bs     =  a      :appendByIndex as bs
appendByIndex    as  (b:bs) =       b :appendByIndex as bs
appendByIndex    []     []  = []
