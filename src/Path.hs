module Path (
    fromText,
    path,  -- FIXME: bad interface
    method,   -- FIXME: bad interface
    toString,
    ) where

import Data.Char (isLetter, isDigit)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))
import qualified Data.Text as T

import Data.Attoparsec.Text (
    Parser, parseOnly, endOfInput, char, letter, satisfy, many')

import Util (parseType, uncamel)
import Types (ClapiPath, ClapiMethod)


root :: ClapiPath
root = []

up :: ClapiPath -> ClapiPath
up [] = root
-- FIXME: using Data.Seq would be faster than a built in list for init (removing
-- last element)
up cs = init cs


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

method :: Parser ClapiMethod
method = parseType uncamel

path :: Parser ClapiPath
path = many' (pathSeparator >> pathComponent) <|> (pathSeparator >> return root)

toString :: ClapiPath -> String
toString [] = "/"
toString cs = concatMap (sepChar :) cs

fromText :: T.Text -> Either String ClapiPath
fromText = parseOnly (path <* endOfInput)
