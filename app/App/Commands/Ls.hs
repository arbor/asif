{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Ls
  ( commandLs
  ) where

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
import Data.Thyme.Clock.POSIX         (POSIXTime)
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

      forM_ (zip [0..] namedSegments) $ \(i :: Int, (filename, segment)) -> do
        let meta = segment ^. the @"meta"

        liftIO $ IO.hPutStrLn hOut $ mempty
          <> (show i & trimPadLeft 4)
          <> " " <> (meta     ^. the @"createTime" <&> showPosixSeconds & fromMaybe ""  & trimPadLeft   19)
          <> " " <> (meta     ^. the @"format"     <&> show             & fromMaybe ""  & trimPadRight  20)
          <> " " <> (segment  ^. the @"payload"    &   showLbsLength                    & trimPadLeft   12)
          <> " " <> T.unpack filename

  where magic = AP.string "seg:" *> (BS.pack <$> many AP.anyWord8) AP.<?> "\"seg:????\""

showLbsLength :: LBS.ByteString -> String
showLbsLength = show . LBS.length

showPosixSeconds :: POSIXTime -> String
showPosixSeconds t = take 19 (show (posixSecondsToUTCTime t))

trimPadLeft :: Int -> String -> String
trimPadLeft n s
  | len < n   = replicate (n - len) ' ' <> s
  | len == n  = s
  | otherwise = '<':reverse (take (n - 1) (reverse s))
  where len = length s

trimPadRight :: Int -> String -> String
trimPadRight n s
  | len < n   = s <> replicate (n - len) ' '
  | len == n  = s
  | otherwise = take (n - 1) s <> ">"
  where len = length s
