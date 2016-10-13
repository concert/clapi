module Path (
        BasePath (..),
        fromOsc,
        toOsc,
    ) where

import Data.Char (isLetter, isDigit)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Text.Parsec (char, satisfy, letter, many, eof, parse, ParseError)
import Text.Parsec.String (Parser)

import Util (parseType, uncamel)
import Types (ClapiMethod)


data BasePath = BasePath {components :: [String]} deriving (Eq, Show)
root = BasePath []

up :: BasePath -> BasePath
up (BasePath []) = root
-- FIXME: using Data.Seq would be faster than a built in list for init (removing
-- last element)
up (BasePath cs) = BasePath (init cs)

{- FIXME: perhaps we make distinction between the human-friendly form we want to
display and an eventual binary serialisation? -}
class OscSerialisable a where
    toOsc :: a -> String
    fromOsc :: String -> Either ParseError a


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

basePath :: Parser BasePath
basePath = do
    pathSeparator
    components <- many pcDropSep
    return (BasePath components)
    where
        pcDropSep = do
            pc <- pathComponent
            pathSeparator
            return pc

instance OscSerialisable BasePath where
    -- FIXME: I don't think this is very efficient!
    toOsc (BasePath components) = "/" ++ intercalate "/" components ++ "/"
    fromOsc = parse (basePath <* eof) ""
