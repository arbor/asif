module Arbor.File.Format.Asif.Format where

data Format
  = Binary
  | BitString
  | Int8LE
  | Int16LE
  | Int32LE
  | Int64LE
  | Ipv4
  | StringZ
  | Text
  | TimeMicros
  | Word8LE
  | Word16LE
  | Word32LE
  | Word64LE
  deriving (Eq, Read, Show)
