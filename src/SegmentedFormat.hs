{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module SegmentedFormat
( CountryFeed
, AsnFeed
, NaicFeed
, binarySearch
, extractSegments
, getCountryCodeMap
, identifiers
, loadAsnFeed
, loadCountryFeed
, loadNaicFeed
, lookupCidr
, lookupIdentifier
, lookupValue
, names
, segmentCidrs
, segmentCodes
, segmentIdentifiers
, cidrStarts
, cidrStops
)
where

import Control.Lens
import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Builder
import Data.Either.Combinators
import Data.Int
import Data.List
import Data.Monoid
import Data.Traversable
-- import Data.Vector.Algorithms.Search
import Data.Word

import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Map.Strict            as M
import qualified Data.Vector.Unboxed        as VU

data CountryFeed = CountryFeed
  { _countryFeedCidrStarts  :: VU.Vector Word32
  , _countryFeedCidrStops   :: VU.Vector Word32
  , _countryFeedIdentifiers :: VU.Vector CountryCode
  , _countryFeedNames       :: M.Map CountryCode Name
  }

data AsnFeed = AsnFeed
  { _asnFeedCidrStarts  :: VU.Vector Word32
  , _asnFeedCidrStops   :: VU.Vector Word32
  , _asnFeedIdentifiers :: VU.Vector Word32
  , _asnFeedNames       :: M.Map Word32 Name
  }

data NaicFeed = NaicFeed
  { _naicFeedCidrStarts  :: VU.Vector Word32
  , _naicFeedCidrStops   :: VU.Vector Word32
  , _naicFeedIdentifiers :: VU.Vector Word32
  , _naicFeedNames       :: M.Map Word32 Name
  }

type Name = String
type CountryCode = (Char, Char)

makeFields ''CountryFeed
makeFields ''AsnFeed
makeFields ''NaicFeed

type HasCidrs feed el = (HasCidrStarts feed (VU.Vector el)
                        , HasCidrStops  feed (VU.Vector el)
                        , VU.Unbox el
                        , Ord el)

type HasValues feed el = (VU.Unbox el, HasIdentifiers feed (VU.Vector el))

getMagic :: LC8.ByteString -> Get ()
getMagic m = do
  a <- getLazyByteString 8
  when (a /= m) $
    fail $ "wrong magic: " <> LC8.unpack a <> " expected: " <> LC8.unpack m

getVersion :: Get Word64
getVersion = getWord64le

getSegmentLength :: Get Int
getSegmentLength = fromIntegral <$> getInt64le

getSegmentPosition :: Get (Int, Int)
getSegmentPosition = (,)
  <$> (fromIntegral <$> getInt32le)
  <*> (fromIntegral <$> getInt32le)

getSegmentPositions :: Get [(Int, Int)]
getSegmentPositions = do
  n <- getSegmentLength
  replicateM n getSegmentPosition

getHeader :: LC8.ByteString -> Get [(Int, Int)]
getHeader m = do
  getMagic m
  getVersion
  getSegmentPositions

extractSegments :: LC8.ByteString -> LBS.ByteString -> Either String [LBS.ByteString]
extractSegments m bs = case runGetOrFail (getHeader m) bs of
  Left (_, _, err) -> Left err
  Right (_, _, header) -> do
    let segs = fmap (\(o, l) -> LBS.take (fromIntegral l) $ LBS.drop (fromIntegral o) bs) header
    forM_ (zip segs header) $ \(seg, (_, len)) ->
      when (LC8.length seg /= fromIntegral len) $
        fail "XXX segments not read correctly"
    return segs

segmentElements :: VU.Unbox a => Get a -> LBS.ByteString -> VU.Vector a
segmentElements f = VU.unfoldr step
  where step !s = case runGetOrFail f s of
          Left (_, _, err)    -> Nothing
          Right (!rem, _, !k) -> Just (k, rem)

segmentElements' :: Get a -> LBS.ByteString -> [a]
segmentElements' f = unfoldr step
  where step !s = case runGetOrFail f s of
          Left (_, _, err)    -> Nothing
          Right (!rem, _, !k) -> Just (k, rem)

segmentCidrs :: LBS.ByteString -> VU.Vector Word32
segmentCidrs = segmentElements getWord32le

getCountryCode :: Get CountryCode
getCountryCode = do
  a <- getByteString 1
  b <- getByteString 1
  let [a'] = C8.unpack a
  let [b'] = C8.unpack b
  return (a', b')

getCountryCodeMap :: LBS.ByteString -> LBS.ByteString -> M.Map CountryCode String
getCountryCodeMap kbs vbs = segmentMap kbs getCountryCode vbs getNullString

getNameMap :: LBS.ByteString -> LBS.ByteString -> M.Map Word32 String
getNameMap kbs vbs = segmentMap kbs getWord32le vbs getNullString

getNullString :: Get String
getNullString = LC8.unpack <$> getLazyByteStringNul

segmentCodes :: LBS.ByteString -> VU.Vector CountryCode
segmentCodes = segmentElements getCountryCode

segmentIdentifiers :: LBS.ByteString -> VU.Vector Word32
segmentIdentifiers = segmentElements getWord32le

segmentMap :: (Ord a) => LBS.ByteString -> Get a -> LBS.ByteString -> Get b -> M.Map a b
segmentMap ks kf vs vf = foldr (\(k, v) m -> M.insert k v m) M.empty $ zip keys values
  where
    keys = segmentElements' kf ks
    values = segmentElements' vf vs

loadCountryFeed :: LBS.ByteString -> Either String CountryFeed
loadCountryFeed bs =
  case extractSegments "seg:ganc" bs of
    Right [a, b, c, ks, vs] -> Right $
     CountryFeed (segmentCidrs a) (segmentCidrs b) (segmentCodes c) (getCountryCodeMap ks vs)
    Right lst               -> Left ("Missing or extra segments: " <> show (length lst))
    Left e                  -> Left e

loadAsnFeed :: LBS.ByteString -> Either String AsnFeed
loadAsnFeed bs =
  case extractSegments "seg:gana" bs of
    Right [a, b, c, ks, vs] -> Right $
     AsnFeed (segmentCidrs a) (segmentCidrs b) (segmentIdentifiers c) (getNameMap ks vs)
    Right lst               -> Left ("Missing or extra segments: " <> show (length lst))
    Left e                  -> Left e

loadNaicFeed :: LBS.ByteString -> Either String NaicFeed
loadNaicFeed bs =
  case extractSegments "seg:gann" bs of
    Right [a, b, c, ks, vs] -> Right $
     NaicFeed (segmentCidrs a) (segmentCidrs b) (segmentIdentifiers c) (getNameMap ks vs)
    Right lst               -> Left ("Missing or extra segments: " <> show (length lst))
    Left e                  -> Left e

binarySearch :: (Ord a, VU.Unbox a) => a -> VU.Vector a -> Maybe Int
binarySearch key values = do
  let idx = s key values 0 (VU.length values - 1)
  if idx > -1 then Just idx
  else Nothing
  where
    s :: (Ord a, VU.Unbox a) => a -> VU.Vector a -> Int -> Int -> Int
    s k v l h
      | l >= h =
        if (v VU.! h) <= k then h else -1
      | otherwise = do
        let m = l + (h - l) `div` 2
        if (v VU.! m) > k then s k v l m
        else do
          let result = s k v (m + 1) h
          if result == -1 then m
          else result

-- get the index of the greatest element not exceeding the key (cidrStarts)
-- compare the key against the corresponding cidr stop (by index)
-- if <= stop, we have found a match. otherwise, a gap.
lookupCidr :: HasCidrs f c
  => f
  -> c
  -> Maybe Int
lookupCidr feed cidr = do
  i <- binarySearch cidr $ feed ^. cidrStarts
  -- the cidr range that contains the search cidr
  let stop = (feed ^. cidrStops) VU.! i
  if cidr <= stop
    then return i
    else Nothing

-- search for a CIDR, then look up its identifier. (country code, naic id, asnum)
lookupIdentifier :: (HasCidrs f c, HasValues f v)
  => f
  -> c
  -> Maybe v
lookupIdentifier feed cidr =
  lookupCidr feed cidr >>=
    (VU.!?) (feed ^. identifiers)

-- search for a CIDR, then, through its identifier, look up its name. (country name, naic name, asn org name)
lookupValue :: (HasCidrs f c, HasValues f k, Ord k)
  => f
  -> c
  -> Getting (M.Map k v) f (M.Map k v)
  -> Maybe v
lookupValue feed cidr lens =
  lookupCidr feed cidr >>=
    (VU.!?) (feed ^. identifiers) >>=
       flip M.lookup (feed ^. lens)
