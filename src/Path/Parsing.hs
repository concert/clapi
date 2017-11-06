module Path.Parsing (toString, fromString, pathP, nameP) where

import Data.Char (isLetter, isDigit)
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Control.Monad.Fail (MonadFail)

import Data.Attoparsec.Text (
    Parser, char, letter, satisfy, many1, sepBy, parseOnly)

import Clapi.Util (eitherFail)
import Clapi.Path (Path, root)

sepChar = '/'

separatorP :: Parser Char
separatorP = char sepChar

isValidPathSegmentChar :: Char -> Bool
isValidPathSegmentChar c = isLetter c || isDigit c || c == '_'

nameP :: Parser String
nameP = many1 $ satisfy isValidPathSegmentChar

pathP :: Parser Path
pathP = separatorP >> nameP `sepBy` separatorP

toString :: Path -> String
toString [] = [sepChar]
toString cs = concatMap (sepChar :) cs

fromString :: (MonadFail m) => String -> m Path
fromString s = eitherFail $ parseOnly pathP (T.pack s)
