module Util (
    eitherFail,
    tag,
    append, (+|),
    appendIfAbsent, (+|?),
    zipLongest,
    camel,
    uncamel,
    parseType,
    composeParsers,
) where

import Prelude hiding (fail)
import Data.Char (isUpper, toLower, toUpper)
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Monad.Fail (MonadFail, fail)

import qualified Data.Attoparsec.Internal.Types as I
import qualified Data.Attoparsec.Text as APT
import qualified Data.Attoparsec.ByteString as APBS

eitherFail :: (MonadFail m) => Either String a -> m a
eitherFail = either fail return

tag :: (Functor f) => (a -> b) -> f a -> f (b, a)
tag f = fmap (\a -> (f a, a))

append :: [a] -> a -> [a]
append as a = as ++ [a]
(+|) = append

appendIfAbsent :: (Eq a) => [a] -> a -> [a]
appendIfAbsent as a | a `elem` as = as
                    | otherwise = append as a
(+|?) :: (Eq a) => [a] -> a -> [a]
(+|?) = appendIfAbsent

zipLongest :: (Monoid a, Monoid b) => [a] -> [b] -> [(a, b)]
zipLongest [] [] = []
zipLongest [] (b:bs) = (mempty, b) : zipLongest [] bs
zipLongest (a:as) [] = (a, mempty) : zipLongest as []
zipLongest (a:as) (b:bs) = (a, b) : zipLongest as bs


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

parseType :: (Show a, Enum a, Bounded a) => (String -> String) -> APT.Parser a
parseType transform =
    let m = typeNameMap transform in
    do
        match <- APT.choice $ map (APT.try . APT.string . T.pack) $ Map.keys m
        -- The parse will already fail if we don't have a match, so we can safely
        --  unwrap the Maybe:
        return $ fromJust $ Map.lookup (T.unpack match) m

class ParseOnlyAble a where
    doParseOnly :: I.Parser a b -> a -> Either String b

instance ParseOnlyAble ByteString where
    doParseOnly = APBS.parseOnly

instance ParseOnlyAble T.Text where
    doParseOnly = APT.parseOnly

composeParsers :: ParseOnlyAble b =>
    I.Parser a b -> I.Parser b c -> I.Parser a c
composeParsers parserA parserB = do
    a <- parserA
    handleResult $ doParseOnly parserB a
  where
    handleResult (Left errStr) = fail errStr
    handleResult (Right x) = return x
