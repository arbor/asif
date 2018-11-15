{-# LANGUAGE DeriveGeneric #-}

module Arbor.File.Format.Asif.Type where

import           Arbor.File.Format.Asif.Format.Type (Format)
import           Arbor.File.Format.Asif.Maybe
import           Arbor.File.Format.Asif.Whatever
import           Data.Semigroup                     (Semigroup, (<>))
import           Data.Text                          (Text)
import           Data.Thyme.Clock.POSIX             (POSIXTime)
import           GHC.Generics

data SegmentMeta = SegmentMeta
  { createTime :: Maybe POSIXTime
  , filename   :: Maybe Text
  , format     :: Maybe (Whatever Format)
  } deriving (Eq, Show, Generic)

instance Semigroup SegmentMeta where
  a <> b =  SegmentMeta
    { createTime = createTime a `secondJust` createTime b
    , filename   = filename   a `secondJust` filename   b
    , format     = format     a `secondJust` format     b
    }

instance Monoid SegmentMeta where
  mappend = (<>)
  mempty = SegmentMeta
    { createTime = Nothing
    , filename   = Nothing
    , format     = Nothing
    }

data Segment a = Segment
  { meta    :: SegmentMeta
  , payload :: a
  } deriving Generic

segment :: a -> SegmentMeta -> Segment a
segment payload' meta' = Segment
  { meta      = meta'
  , payload   = payload'
  }

metaCreateTime :: POSIXTime -> SegmentMeta
metaCreateTime time = mempty
  { createTime = Just time
  }

metaFilename :: Text -> SegmentMeta
metaFilename filePath = mempty
  { filename = Just filePath
  }

metaFormat :: Whatever Format -> SegmentMeta
metaFormat format' = mempty
  { format = Just format'
  }

metaMaybeFormat :: Maybe (Whatever Format) -> SegmentMeta
metaMaybeFormat maybeFormat = mempty
  { format = maybeFormat
  }
