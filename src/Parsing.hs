module Parsing (
    pathToString,
    pathParser,
    methodToString,
    methodParser,
    ) where

import Data.Char (isLetter, isDigit)
import Control.Applicative ((<|>))

import Data.Attoparsec.Text (Parser, char, letter, satisfy, many')

import Util (parseType, uncamel)
import Types (ClapiPath, root, ClapiMethod)

sepChar = '/'

pathSeparator :: Parser Char
pathSeparator = char sepChar

pathComponent :: Parser String
pathComponent = do
    first <- firstChar
    rest <- many' restChar
    return (first:rest)
    where
        firstChar = letter
        restChar = satisfy (\c -> isLetter c || isDigit c || c == '_')

separatedPathComponent :: Parser String
separatedPathComponent = pathSeparator >> pathComponent

pathParser :: Parser ClapiPath
pathParser = many' (pathSeparator >> pathComponent) <|> (pathSeparator >> return root)

pathToString :: ClapiPath -> String
pathToString [] = "/"
pathToString cs = concatMap (sepChar :) cs

methodParser :: Parser ClapiMethod
methodParser = parseType uncamel

methodToString :: ClapiMethod -> String
methodToString = uncamel . show
