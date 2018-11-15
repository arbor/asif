module Arbor.File.Format.Asif.List where

import           Data.List

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . unfoldr (Just . splitAt n)
