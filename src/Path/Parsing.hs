module Path.Parsing (toString, pathP, nameP) where

import Data.Char (isLetter, isDigit)
import Control.Applicative ((<|>))

import Data.Attoparsec.Text (Parser, char, letter, satisfy, many')

import Path (Path, root)

sepChar = '/'

separatorP :: Parser Char
separatorP = char sepChar

nameP :: Parser String
nameP = do
    first <- firstChar
    rest <- many' restChar
    return (first:rest)
    where
        firstChar = letter
        restChar = satisfy (\c -> isLetter c || isDigit c || c == '_')

pathP :: Parser Path
pathP = many' (separatorP >> nameP) <|> (separatorP >> return root)

toString :: Path -> String
toString [] = [sepChar]
toString cs = concatMap (sepChar :) cs
