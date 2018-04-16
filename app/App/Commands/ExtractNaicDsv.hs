module App.Commands.ExtractNaicDsv where

import Options.Applicative

commandExtractNaicDsv :: Parser (IO ())
commandExtractNaicDsv = runExtractNaicDsv <$> argument str idm

runExtractNaicDsv :: String -> IO ()
runExtractNaicDsv _ = return ()
