module Gen.Feed
  ( feedElement
  , feedElements
  , ipv4
  , ordBy
  ) where

import Data.Word
import HaskellWorks.Data.Network.Ip.Internal.Appar
import HaskellWorks.Data.Network.Ip.Ipv4
import Hedgehog

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

ipv4 :: MonadGen m => Word8 -> Word8 -> m IpAddress
ipv4 start stop = IpAddress <$> (fourOctetsToWord32 <$> w8 <*> w8 <*> w8 <*> w8)
  where w8 = G.word8 (R.linear start stop)

feedElemIp :: MonadGen m => Range Word32 -> m Word32
feedElemIp = G.word32

feedElemValue :: MonadGen m => Range Word64 -> m Word64
feedElemValue = G.word64

feedElement :: MonadGen m => Word32 -> Word32 ->  m (Word32, Word32, Word64)
feedElement from to = do
  let mid = to - (to - from) `div` 2
  (,,) <$> feedElemIp (R.linear from mid)
       <*> feedElemIp (R.linear mid to)
       <*> feedElemValue R.linearBounded

feedElements :: MonadGen m => Word32 -> Word32 -> m [(Word32, Word32, Word64)]
feedElements from to = G.list (R.linear 0 100) $ feedElement from to

ordBy :: Ord k => (a -> k) -> a -> a -> Ordering
ordBy f a b = compare (f a) (f b)
