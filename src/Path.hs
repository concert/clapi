module Path (
    fromString,
    toString,
    ) where

import Data.Char (isLetter, isDigit)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Text.Parsec (char, satisfy, letter, many, eof, parse, ParseError)
import Text.Parsec.String (Parser)

import Util (parseType, uncamel)
import Types (ClapiPath, ClapiMethod)


root :: ClapiPath
root = []

up :: ClapiPath -> ClapiPath
up [] = root
-- FIXME: using Data.Seq would be faster than a built in list for init (removing
-- last element)
up cs = init cs


pathSeparator :: Parser Char
pathSeparator = char '/'

pathComponent :: Parser String
pathComponent = do
    first <- firstChar
    rest <- many restChar
    return (first:rest)
    where
        firstChar = letter
        restChar = satisfy (\c -> isLetter c || isDigit c || c == '_')

method :: Parser ClapiMethod
method = parseType uncamel

path :: Parser ClapiPath
path = do
    pathSeparator
    components <- many pcDropSep
    return components
    where
        pcDropSep = do
            pc <- pathComponent
            pathSeparator
            return pc

toString :: ClapiPath -> String
-- FIXME: I don't think this is very efficient!
toString components = "/" ++ intercalate "/" components ++ "/"

fromString :: String -> Either ParseError ClapiPath
fromString = parse (path <* eof) ""
