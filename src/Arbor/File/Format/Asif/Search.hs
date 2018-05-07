module Arbor.File.Format.Asif.Search where

import Arbor.File.Format.Asif.ByteString.Builder
import Arbor.File.Format.Asif.Format             (Format)
import Arbor.File.Format.Asif.Get
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

binarySearch :: (Ord a, VU.Unbox a) => a -> VU.Vector a -> Maybe Int
binarySearch key values = do
  guard (not (VU.null values))
  let idx = s 0 (VU.length values - 1)
  guard (idx > -1)
  return idx
  where
    s l h
      | l >= h =
        if (values VU.! h) <= key then h else -1
      | otherwise = do
        let m = l + (h - l) `div` 2
        if (values VU.! m) > key then s l m
        else do
          let result = s (m + 1) h
          if result == -1 then m
          else result

binarySearchExact :: (Ord a, VU.Unbox a) => a -> VU.Vector a -> Maybe Int
binarySearchExact key values = go values key 0 (VU.length values - 1)
  where
    go hay needle lo hi
      | hi < lo        = Nothing
      | pivot > needle = go hay needle lo (mid - 1)
      | pivot < needle = go hay needle (mid + 1) hi
      | otherwise      = Just mid
      where
        mid   = lo + (hi - lo) `div` 2
        pivot = hay VU.! mid
