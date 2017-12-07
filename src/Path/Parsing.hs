module Path.Parsing (toString, toText, fromString, fromText, pathP, nameP) where

import Data.Char (isLetter, isDigit)
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Control.Monad.Fail (MonadFail)

import Data.Attoparsec.Text (
    Parser, char, letter, satisfy, many1, sepBy, parseOnly)

import Clapi.Util (eitherFail)
import Clapi.Path (Name, Path(..), root)

sepChar = '/'
sepText = T.singleton sepChar

separatorP :: Parser Char
separatorP = char sepChar

isValidPathSegmentChar :: Char -> Bool
isValidPathSegmentChar c = isLetter c || isDigit c || c == '_'

nameP :: Parser Name
nameP = fmap T.pack $ many1 $ satisfy isValidPathSegmentChar

pathP :: Parser Path
pathP = fmap Path $ separatorP >> nameP `sepBy` separatorP

toText :: Path -> T.Text
toText (Path []) = sepText
toText (Path cs) = T.append sepText $ T.intercalate sepText cs

toString :: Path -> String
toString p = T.unpack $ toText p

fromText :: MonadFail m => T.Text -> m Path
fromText t = eitherFail $ parseOnly pathP t

fromString :: MonadFail m => String -> m Path
fromString s = fromText $ T.pack s
