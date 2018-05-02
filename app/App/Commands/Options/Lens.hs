{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module App.Commands.Options.Lens where

import App.Commands.Options.Type
import Control.Lens

makeFields ''DumpOptions
makeFields ''ExtractFilesOptions
makeFields ''ExtractSegmentsOptions
