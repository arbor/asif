{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Arbor.File.Format.Asif.Lens where

import Arbor.File.Format.Asif.Format
import Arbor.File.Format.Asif.Type
import Control.Lens

makeFields ''Format
makeFields ''Segment
makeFields ''SegmentMeta
