module Arbor.File.Format.Asif.Maybe where

secondJust :: Maybe a -> Maybe a -> Maybe a
secondJust _ (Just b) = Just b
secondJust (Just a) _ = Just a
secondJust _ _        = Nothing
