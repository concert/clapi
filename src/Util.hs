module Util (
    uncamel
) where

import Data.Char (isUpper, toLower)

uncamel :: String -> String
uncamel [] = []
uncamel (c:cs) = toLower c : uncamel' cs

uncamel' :: String -> String
uncamel' [] = []
uncamel' (c:cs)
    | isUpper c = '_' : toLower c : uncamel' cs
    | otherwise = c : uncamel' cs
