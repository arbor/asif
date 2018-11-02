{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Ls where

import App.Commands.Options.Type
import Arbor.File.Format.Asif.IO
import Arbor.File.Format.Asif.Segment
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Trans.Resource   (MonadResource, runResourceT)
import Data.Generics.Product.Any
import Data.Maybe
import Data.Monoid                    ((<>))
import Data.Thyme.Format              (formatTime)
import Data.Thyme.Time.Core
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import Options.Applicative
import System.Locale                  (defaultTimeLocale, iso8601DateFormat)

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Binary                as G
import qualified Data.Binary.Get            as G
import qualified Data.Bits                  as B
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import qualified System.IO                  as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

parseLsOptions :: Parser LsOptions
parseLsOptions = LsOptions
  <$> strOption
      (   long "source"
      <>  metavar "FILE"
      <>  value "-"
      <>  help "Input file"
      )
  <*> strOption
      (   long "target"
      <>  metavar "FILE"
      <>  value "-"
      <>  help "Output file"
      )

commandLs :: Parser (IO ())
commandLs = runResourceT . runDump <$> parseLsOptions

showTime :: FormatTime t => t -> String
showTime = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S %Z"))

getWord32x4 :: G.Get (Word32, Word32, Word32, Word32)
getWord32x4 = do
  a <- G.getWord32be
  b <- G.getWord32be
  c <- G.getWord32be
  d <- G.getWord32be
  return (a, b, c, d)

runDump :: MonadResource m => LsOptions -> m ()
runDump opt = do
  (_, hIn)  <- openFileOrStd (opt ^. the @"source") IO.ReadMode
  (_, hOut) <- openFileOrStd (opt ^. the @"target") IO.WriteMode

  contents <- liftIO $ LBS.hGetContents hIn

  case extractSegments magic contents of
    Left errorMessage -> do
      liftIO $ IO.hPutStrLn IO.stderr $ "Error occured: " <> errorMessage
      return ()
    Right segments -> do
      let filenames = fromMaybe "" . (^. the @"meta" . the @"filename") <$> segments
      let namedSegments = zip filenames segments

      forM_ (zip [0..] namedSegments) $ \(i :: Int, (filename, _)) -> do
        liftIO $ IO.hPutStrLn hOut $ "#" <> show i <> " " <> T.unpack filename

  where magic = AP.string "seg:" *> (BS.pack <$> many AP.anyWord8) AP.<?> "\"seg:????\""

word64ToList :: Int -> Word64 -> [Word32] -> [Word32]
word64ToList _ 0 = id
word64ToList o w = (ip:) . word64ToList o (w .&. comp b)
  where p  = B.countTrailingZeros w
        hi = o .<. 6
        ip = fromIntegral (p .|. hi)
        b  = 1 .<. fromIntegral p
