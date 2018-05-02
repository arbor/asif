module Arbor.File.Format.Asif.Format where

data Format
  = Binary
  | BitString
  | Char
  | Int16LE
  | Int32LE
  | Int64LE
  | Int8LE
  | Ipv4
  | Repeat Int Format
  | StringZ
  | Text
  | TimeMicros
  | Word16LE
  | Word32LE
  | Word8LE
  | Word64LE
  deriving (Eq, Read, Show)
