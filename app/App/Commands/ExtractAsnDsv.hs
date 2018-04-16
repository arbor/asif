module App.Commands.ExtractAsnDsv
  ( commandExtractAsnDsv
  ) where

import Options.Applicative

commandExtractAsnDsv :: Parser (IO ())
commandExtractAsnDsv = runExtractAsnDsv <$> argument str idm

runExtractAsnDsv :: String -> IO ()
runExtractAsnDsv _ = return ()
