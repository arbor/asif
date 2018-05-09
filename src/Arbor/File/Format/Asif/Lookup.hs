module Arbor.File.Format.Asif.Lookup where

import Control.Monad
import Data.Text     (Text)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as M
import qualified Data.Vector.Unboxed  as VU

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

lookupSegment :: Text -> M.Map Text LBS.ByteString -> (LBS.ByteString -> [a]) -> [a]
lookupSegment filename directory f = case M.lookup filename directory of
  Just bs -> f bs
  Nothing -> []
