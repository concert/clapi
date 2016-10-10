module Util (
    camel,
    uncamel,
    parseType
) where

import Data.Char (isUpper, toLower, toUpper)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

import Text.Parsec (choice, try, string)
import Text.Parsec.String (Parser)

uncamel :: String -> String
uncamel [] = []
uncamel (c:cs) = toLower c : uncamel' cs where
    uncamel' :: String -> String
    uncamel' [] = []
    uncamel' (c:cs)
        | isUpper c = '_' : toLower c : uncamel' cs
        | otherwise = c : uncamel' cs

camel :: String -> String
camel = (foldl (++) "") . (map initCap) . (splitOn "_") where
    initCap [] = []
    initCap (c:cs) = toUpper c : cs


typeNameMap ::
    (Show a, Enum a, Bounded a) => (String -> String) -> Map.Map String a
typeNameMap transform =
    Map.fromList [(transform . show $ constructor, constructor) | constructor <- [minBound ..]]

{- FIXME: add tests for typeNameMap. Along the lines of:
data Test = One | Two | Three deriving (Enum, Show, Bounded)

go :: Map.Map String Test
go = typeNameMap (\a -> a)
-}

parseType :: (Show a, Enum a, Bounded a) => (String -> String) -> Parser a
parseType transform =
    let m = typeNameMap transform in
    do
        match <- choice $ map (try . string) $ Map.keys m
        -- The parse will already fail if we don't have a match, so we can safely
        --  unwrap the Maybe:
        return $ fromJust $ Map.lookup match m
