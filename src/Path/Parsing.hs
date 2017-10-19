module Path.Parsing (toString, fromString, pathP, nameP) where

import Data.Char (isLetter, isDigit)
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Control.Monad.Fail (MonadFail)

import Data.Attoparsec.Text (Parser, char, letter, satisfy, many', parseOnly)

import Clapi.Util (eitherFail)
import Clapi.Path (Path, root)

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

fromString :: (MonadFail m) => String -> m Path
fromString s = eitherFail $ parseOnly pathP (T.pack s)
