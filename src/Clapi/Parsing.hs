module Clapi.Parsing (
    methodToString,
    methodParser,
    ) where

import Data.Attoparsec.Text (Parser)

import Clapi.Util (parseType, uncamel)
import Clapi.Types (ClapiMethod)


methodParser :: Parser ClapiMethod
methodParser = parseType uncamel

methodToString :: ClapiMethod -> String
methodToString = uncamel . show
