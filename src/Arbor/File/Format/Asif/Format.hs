module Arbor.File.Format.Asif.Format where

data Format
  = Binary
  | BitString
  | Char
  | Int8
  | Int16LE
  | Int32LE
  | Int64LE
  | Ipv4
  | Repeat Word Format
  | StringZ
  | Text
  | TimeMicros64LE
  | TimeMillis64LE
  | Word8
  | Word16LE
  | Word32LE
  | Word64LE
  deriving (Eq, Read, Show)
