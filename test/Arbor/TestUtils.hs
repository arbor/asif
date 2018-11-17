module Arbor.TestUtils
where

import qualified Data.List as L

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = L.takeWhile (not . L.null) . L.unfoldr (Just . L.splitAt n)

