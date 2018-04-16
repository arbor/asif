module App.Commands.LookupCountry where

import Options.Applicative

commandLookupCountry :: Parser (IO ())
commandLookupCountry = runLookupCountry <$> argument str idm

runLookupCountry :: String -> IO ()
runLookupCountry _ = return ()
