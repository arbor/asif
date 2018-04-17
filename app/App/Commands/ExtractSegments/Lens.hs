{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module App.Commands.ExtractSegments.Lens where

import App.Commands.ExtractSegments.Type
import Control.Lens

makeFields ''CommandOptions
