{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module App.Commands.ExtractCountryDsv.Lens where

import App.Commands.ExtractCountryDsv.Type
import Control.Lens

makeFields ''CommandOptions
