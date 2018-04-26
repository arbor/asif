module Arbor.File.Format.Asif.Type where

import Arbor.File.Format.Asif.Maybe
import Data.Monoid
import Data.Text                    (Text)
import Data.Thyme
import Data.Thyme.Clock.POSIX       (POSIXTime, getPOSIXTime)

import qualified System.IO as IO

data SegmentMeta = SegmentMeta
  { _segmentMetaCreateTime :: Maybe POSIXTime
  , _segmentMetaFilename   :: Maybe Text
  } deriving (Eq, Show)

instance Monoid SegmentMeta where
  a `mappend` b =  SegmentMeta
    { _segmentMetaCreateTime = _segmentMetaCreateTime a `firstJust` _segmentMetaCreateTime b
    , _segmentMetaFilename   = _segmentMetaFilename   a `firstJust` _segmentMetaFilename   b
    }
  mempty = SegmentMeta
    { _segmentMetaCreateTime = Nothing
    , _segmentMetaFilename   = Nothing
    }

data Segment a = Segment
  { _segmentMeta    :: SegmentMeta
  , _segmentPayload :: a
  }

segment :: a -> SegmentMeta -> Segment a
segment payload meta = Segment
  { _segmentMeta      = meta
  , _segmentPayload   = payload
  }

metaCreateTime :: POSIXTime -> SegmentMeta
metaCreateTime time = mempty
  { _segmentMetaCreateTime = Just time
  }

metaFilename :: Text -> SegmentMeta
metaFilename filePath = mempty
  { _segmentMetaFilename = Just filePath
  }
