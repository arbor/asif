{-# LANGUAGE DeriveGeneric #-}

module Arbor.File.Format.Asif.Format.Type
where

import           GHC.Generics

data Format
  = Binary
  | Bitmap
  | BitString
  | Char
  | Int8
  | Int16LE
  | Int32LE
  | Int64LE
  | Ipv4
  | Ipv6
  | Repeat Word Format
  | StringZ
  | Text
  | TimeMicros64LE
  | TimeMillis64LE
  | Word8
  | Word16LE
  | Word32LE
  | Word64LE
  deriving (Eq, Read, Show, Generic)
