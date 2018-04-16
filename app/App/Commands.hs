module App.Commands
  ( globalOptions
  ) where

import App.Commands.ExtractAsnDsv
import App.Commands.ExtractCountryDsv
import App.Commands.ExtractNaicDsv
import App.Commands.ExtractSegments
import Data.Monoid
import Options.Applicative

globalOptions :: Parser (IO ())
globalOptions = subparser
  (   command "extract-segments"    (info commandExtractSegments    idm)
  <>  command "extract-asn-dsv"     (info commandExtractAsnDsv      idm)
  <>  command "extract-naic-dsv"    (info commandExtractNaicDsv     idm)
  <>  command "extract-country-dsv" (info commandExtractCountryDsv  idm)
  <>  command "lookup-asn"          (info commandExtractCountryDsv  idm)
  <>  command "lookup-naic"         (info commandExtractCountryDsv  idm)
  <>  command "lookup-country"      (info commandExtractCountryDsv  idm)
  )
