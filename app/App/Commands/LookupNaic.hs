module App.Commands.LookupNaic where

import Options.Applicative

commandLookupNaic :: Parser (IO ())
commandLookupNaic = runLookupNaic <$> argument str idm

runLookupNaic :: String -> IO ()
runLookupNaic _ = return ()
