module App.Commands.ExtractCountryDsv
  ( commandExtractCountryDsv
  ) where

import App.Commands.ExtractCountryDsv.Type
import Control.Lens
import Control.Monad
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Word
import SegmentedFormat
import HaskellWorks.Data.Bits.BitWise
import Options.Applicative

import qualified App.Commands.ExtractCountryDsv.Lens as L
import qualified App.IO                              as IO
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Lazy.Char8          as LBSC
import qualified Data.Vector.Unboxed                 as VU
import qualified System.IO                           as IO

parseCommandOptions :: Parser CommandOptions
parseCommandOptions = CommandOptions
  <$> strOption
      (   long "source"
      <>  metavar "FILE"
      <>  help "Input file"
      )
  <*> optional
      ( strOption
        (   long "target"
        <>  metavar "FILE"
        <>  help "Output file"
        )
      )

commandExtractCountryDsv :: Parser (IO ())
commandExtractCountryDsv = runExtractCountryDsv <$> parseCommandOptions

runExtractCountryDsv :: CommandOptions -> IO ()
runExtractCountryDsv opt = do
  let targetPath = opt ^. L.target & fromMaybe "-"
  hInput <- IO.openFile (opt ^. L.source) IO.ReadMode
  contents <- LBS.hGetContents hInput
  case extractSegments "seg:ganc" contents of
    Left error -> do
      IO.hPutStrLn IO.stderr $ "Error occured: " <> show error
      return ()
    Right (bsIpFirst:bsIpLast:bsIpCc:_) -> IO.withFileOrStd targetPath IO.WriteMode $ \hOutput -> do
      let rows = zip3 (VU.toList $ segmentCidrs bsIpFirst)
                      (VU.toList $ segmentCidrs bsIpLast )
                      (VU.toList $ segmentCodes bsIpCc   )

      forM_ rows $ \(ipFirst, ipLast, ipCc) -> do
        IO.hPutStrLn hOutput $ show ipFirst <> "|" <> show ipLast <> "|" <> [fst ipCc, snd ipCc]
        return ()

      return ()
