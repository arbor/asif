{-# LANGUAGE DeriveGeneric #-}

module Arbor.File.Format.Asif.Format.Type
where

import GHC.Generics

data Format
  = Binary
  | Bitmap
  | BitString
  | Bool
  | Char
  | Int8
  | Int16LE
  | Int32LE
  | Int64LE
  | Ipv4
  | Ipv6
  | Ipv4Block
  | Ipv6Block
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
