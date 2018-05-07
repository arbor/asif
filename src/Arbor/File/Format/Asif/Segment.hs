module Arbor.File.Format.Asif.Segment
  ( Segment(..)
  , mkDefaultSegment
  , extractSegments
  , extractNamedSegments
  ) where

import Arbor.File.Format.Asif.ByIndex
import Arbor.File.Format.Asif.Extract
import Arbor.File.Format.Asif.Get
import Arbor.File.Format.Asif.Lookup
import Arbor.File.Format.Asif.Type
import Control.Lens
import Control.Monad
import Data.Binary.Get
import Data.Maybe
import Data.Monoid
import Data.Text                      (Text)

import qualified Arbor.File.Format.Asif.Lens as L
import qualified Data.Attoparsec.ByteString  as AP
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LC8
import qualified Data.Map.Strict             as M

mkDefaultSegment :: LBS.ByteString -> Segment LBS.ByteString
mkDefaultSegment bs = segment bs mempty

extractSegments :: AP.Parser BS.ByteString -> LBS.ByteString -> Either String [Segment LBS.ByteString]
extractSegments magicParser bs = do
  bss <- extractSegmentByteStrings magicParser bs
  case bss of
    (as:_) -> if ".asif/filenames\0" `LBS.isPrefixOf` as
      then do
        let filenames     = extractFilenames as
        let namedSegments = M.fromList (zip filenames bss)

        let metas = mempty
              <> ByIndex (replicate (length bss) mempty)
              <> ByIndex (metaFilename    <$> filenames)
              <> ByIndex (metaCreateTime  <$> lookupSegment ".asif/createtimes" namedSegments extractTimes)
              <> ByIndex (metaMaybeFormat <$> lookupSegment ".asif/formats"     namedSegments extractFormats)

        return $ uncurry segment <$> zip bss (unByIndex metas)
      else return (mkDefaultSegment <$> bss)
    _      -> return (mkDefaultSegment <$> bss)

extractNamedSegments :: AP.Parser BS.ByteString -> LBS.ByteString -> Either String (M.Map Text (Segment LBS.ByteString))
extractNamedSegments magicParser bs = do
  segments <- extractSegments magicParser bs
  let filenames = fromMaybe "" . (^. L.meta . L.filename) <$> segments
  return $ M.fromList $ zip filenames segments

extractSegmentByteStrings :: AP.Parser BS.ByteString -> LBS.ByteString -> Either String [LBS.ByteString]
extractSegmentByteStrings magicParser bs = case runGetOrFail (getHeader magicParser) bs of
  Left (_, _, err) -> Left err
  Right (_, _, header) -> do
    let segs = fmap (\(o, l) -> LBS.take (fromIntegral l) $ LBS.drop (fromIntegral o) bs) header
    forM_ (zip segs header) $ \(seg, (_, len)) ->
      when (LC8.length seg /= fromIntegral len) $
        fail "XXX segments not read correctly"
    return segs
