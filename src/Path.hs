module Path (
        BasePath (..),
        Path (..),
        PathMethod (..),
        fromOsc,
        toOsc,
    ) where

import Data.Char (isLetter, isDigit)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

import Text.Parsec (
    char, satisfy, letter, many, many1, lower, try, string, choice, eof, parse,
    ParseError)
import Text.Parsec.String (Parser)

import Util (parseType, uncamel)


data BasePath = BasePath {components :: [String]} deriving (Eq, Show)
root = BasePath []

data PathMethod = Error | Set | Add | Remove | Clear | Subscribe |
    Unsubscribe | AssignType | Children | Delete |
    Identify deriving (Eq, Show, Read, Enum, Bounded)

data Path = Path {base :: BasePath, method :: PathMethod} deriving (Eq, Show)


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

pathMethod :: Parser PathMethod
pathMethod = parseType uncamel

pathMethodSeparator :: Parser Char
pathMethodSeparator = char '#'

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

path :: Parser Path
path = do
    bp <- basePath
    pathMethodSeparator
    method <- pathMethod
    return (Path bp method)

instance OscSerialisable BasePath where
    toOsc (BasePath components) = "/" ++ intercalate "/" components ++ "/"
    fromOsc = parse (basePath <* eof) ""


instance OscSerialisable Path where
    -- FIXME: should eventually be able to serialise method properly
    toOsc (Path base method)= toOsc base ++ "#" ++ show method
    fromOsc = parse (path <* eof) ""
