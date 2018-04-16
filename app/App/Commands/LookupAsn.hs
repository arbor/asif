module App.Commands.LookupAsn where

import Options.Applicative

commandLookupAsn :: Parser (IO ())
commandLookupAsn = runLookupAsn <$> argument str idm

runLookupAsn :: String -> IO ()
runLookupAsn _ = return ()
