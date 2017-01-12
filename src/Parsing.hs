module Parsing (
    methodToString,
    methodParser,
    ) where

import Data.Attoparsec.Text (Parser)

import Util (parseType, uncamel)
import Types (ClapiMethod)


methodParser :: Parser ClapiMethod
methodParser = parseType uncamel

methodToString :: ClapiMethod -> String
methodToString = uncamel . show
