module Lib
    (
        someFunc,
        display,
        parse,
        BasePath (..)
    ) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Char (toLower)

data Failure = Failure { code :: Int, message :: String } deriving (Eq, Show)

class Serialisable a where
  display :: a -> String
  parse :: String -> Either Failure a


data BasePath = BasePath [String] deriving (Eq, Show)
root = BasePath []

instance Serialisable BasePath where
  -- FIXME: I don't think this is very efficient!
  display (BasePath strings) = "/" ++ intercalate "/" strings ++ "/"
  -- FIXME: Actually handle error cases. How?!
  parse string = Right (BasePath $ init . tail $ splitOn "/" string)


data PathMethod = Error | Set | Add | Remove | Clear | Subscribe |
  Unsubscribe | AssignType | Children | Delete | Identify deriving (Eq, Show)
instance Serialisable PathMethod where
  display AssignType = "assign_type"
  display path_method = map toLower $ show path_method
  parse _ = Right Error


data Path = Path { base :: BasePath, method :: PathMethod } deriving (Eq)

instance Serialisable Path where
  -- FIXME: I don't think this is very efficient!
  display (Path base method) = display base ++ "#" ++ display method
  parse _ = Right (path [] Error)

path :: [String] -> PathMethod -> Path
path strings method = Path (BasePath strings) method

up :: Path -> Path
up (Path (BasePath []) method) = Path root method
-- FIXME: using Data.Seq would be faster than a built in list for init (removing
-- last element)
up (Path (BasePath xs) method) = path (init xs) method

someFunc :: IO ()
someFunc = putStrLn $ display $  up $ path ["hello", "world"] Add
