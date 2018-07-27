{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.File.Format.Asif.Segment
  ( Z.Segment(..)
  , mkDefaultSegment
  , extractSegments
  , extractNamedSegments
  , segmentNamed
  ) where

import Arbor.File.Format.Asif.ByIndex
import Arbor.File.Format.Asif.Get
import Arbor.File.Format.Asif.Lookup
import Control.Lens
import Control.Monad
import Data.Binary.Get
import Data.Generics.Product.Any
import Data.Maybe
import Data.Monoid
import Data.Text                      (Text, pack)

import qualified Arbor.File.Format.Asif.Extract as E
import qualified Arbor.File.Format.Asif.Get     as G
import qualified Arbor.File.Format.Asif.Type    as Z
import qualified Data.Attoparsec.ByteString     as AP
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Lazy.Char8     as LC8
import qualified Data.Map.Strict                as M

mkDefaultSegment :: LBS.ByteString -> Z.Segment LBS.ByteString
mkDefaultSegment bs = Z.segment bs mempty

extractSegments :: AP.Parser BS.ByteString -> LBS.ByteString -> Either String [Z.Segment LBS.ByteString]
extractSegments magicParser bs = do
  bss <- extractSegmentByteStrings magicParser bs
  case bss of
    (as:_) -> if ".asif/filenames\0" `LBS.isPrefixOf` as
      then do
        let filenames     = E.list G.getTextUtf8Z as
        let namedSegments = M.fromList (zip filenames bss)

        let metas = mempty
              <> ByIndex (replicate (length bss) mempty)
              <> ByIndex (Z.metaFilename    <$> filenames)
              <> ByIndex (Z.metaCreateTime  <$> lookupSegment ".asif/createtimes" namedSegments (E.list G.getTimeMicro64))
              <> ByIndex (Z.metaMaybeFormat <$> lookupSegment ".asif/formats"     namedSegments E.formats)

        return $ uncurry Z.segment <$> zip bss (unByIndex metas)
      else return (mkDefaultSegment <$> bss)
    _      -> return (mkDefaultSegment <$> bss)

extractNamedSegments :: AP.Parser BS.ByteString -> LBS.ByteString -> Either String (M.Map Text (Z.Segment LBS.ByteString))
extractNamedSegments magicParser bs = do
  segments <- extractSegments magicParser bs
  let filenames = fromMaybe "" . (^. the @"meta" . the @"filename") <$> segments
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

segmentNamed :: String -> M.Map Text (Z.Segment LC8.ByteString) -> Either String LC8.ByteString
segmentNamed name segments = do
  let seg = M.lookup (pack name) segments >>= (\s -> Just (s ^. the @"payload"))
  seg & maybe (Left ("Missing segment: " ++ name)) Right
