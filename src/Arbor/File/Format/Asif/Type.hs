module Arbor.File.Format.Asif.Type where

import Arbor.File.Format.Asif.Format   (Format)
import Arbor.File.Format.Asif.Maybe
import Arbor.File.Format.Asif.Whatever
import Data.Monoid
import Data.Text                       (Text)
import Data.Thyme
import Data.Thyme.Clock.POSIX          (POSIXTime, getPOSIXTime)

import qualified Arbor.File.Format.Asif.Format as F
import qualified System.IO                     as IO

data SegmentMeta = SegmentMeta
  { _segmentMetaCreateTime :: Maybe POSIXTime
  , _segmentMetaFilename   :: Maybe Text
  , _segmentMetaFormat     :: Maybe (Whatever Format)
  } deriving (Eq, Show)

instance Monoid SegmentMeta where
  a `mappend` b =  SegmentMeta
    { _segmentMetaCreateTime = _segmentMetaCreateTime a `secondJust` _segmentMetaCreateTime b
    , _segmentMetaFilename   = _segmentMetaFilename   a `secondJust` _segmentMetaFilename   b
    , _segmentMetaFormat     = _segmentMetaFormat     a `secondJust` _segmentMetaFormat     b
    }
  mempty = SegmentMeta
    { _segmentMetaCreateTime = Nothing
    , _segmentMetaFilename   = Nothing
    , _segmentMetaFormat     = Nothing
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

metaFormat :: Whatever Format -> SegmentMeta
metaFormat format = mempty
  { _segmentMetaFormat = Just format
  }

metaMaybeFormat :: Maybe (Whatever Format) -> SegmentMeta
metaMaybeFormat maybeFormat = mempty
  { _segmentMetaFormat = maybeFormat
  }
