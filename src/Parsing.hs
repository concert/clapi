module Parsing (
    methodToString,
    methodParser,
    ) where

import Data.Char (isLetter, isDigit)
import Control.Applicative ((<|>))

import Data.Attoparsec.Text (Parser, char, letter, satisfy, many')

import Util (parseType, uncamel)
import Types (ClapiMethod)


methodParser :: Parser ClapiMethod
methodParser = parseType uncamel

methodToString :: ClapiMethod -> String
methodToString = uncamel . show
