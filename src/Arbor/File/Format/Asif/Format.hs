module Arbor.File.Format.Asif.Format
  ( module X
  , SegmentValue (..)
  , segmentValues
  , extractValues
  )
where

import           Arbor.File.Format.Asif.Format.Type         as X

import           Arbor.File.Format.Asif.Format.SegmentValue
import           Arbor.File.Format.Asif.Segment
import           Data.ByteString.Lazy                       as LBS

extractValues :: (SegmentValue -> a) -> Segment LBS.ByteString -> [a]
extractValues f s = f <$> segmentValues s
