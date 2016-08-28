module Path
    (
        Failure (..),
        BasePath (..),
        parse
    ) where

import Data.List.Split (splitOn)
import Text.Regex.TDFA ((=~))

class OscSerialisable a where
    toOsc :: a -> String
    fromOsc :: String -> Either Failure a

data Failure = Failure { message :: String } deriving (Eq, Show)

pathComponentPattern = "[A-z0-9][A-z0-9]*"
pathPattern = "^/(" ++ pathComponentPattern ++ "/)*$"

data BasePath = BasePath [String] deriving (Eq, Show)
instance OscSerialisable BasePath where
    toOsc (BasePath strings) = "/" ++ intercalate "/" strings ++ "/"

root = BasePath []

data PathMethod = Error | Set | Add | Remove | Clear | Subscribe |
  Unsubscribe | AssignType | Children | Delete | Identify deriving (Eq, Show)

data Path = Path {base :: BasePath, method :: PathMethod} deriving (Eg, Show)

{- FIXME: Defining three (five?) functions for one expression seems like
overkill! -}
parse_internal ("", s, "") = Right (BasePath $ init . tail $ splitOn "/" s)
parse_internal (s, "", _) = Left $ Failure $ "Invalid path: " ++ s

parse_pattern :: String -> (String, String, String)
parse_pattern s = s =~ pathPattern

parse s = parse_internal $ parse_pattern s
