module Arbor.File.Format.Asif.Maybe where

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust _ (Just b) = Just b
firstJust _ _        = Nothing
