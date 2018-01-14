{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clapi.Util (
    tagl, tagr,
    append, (+|),
    appendIfAbsent, (+|?),
    duplicates, ensureUnique,
    strictZipWith, strictZip, fmtStrictZipError,
    partitionDifference, partitionDifferenceL,
    camel,
    uncamel,
    showItems,
    mkProxy,
    bound,
    safeToEnum
) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail, fail)
import Data.Char (isUpper, toLower, toUpper)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Proxy
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Printf (printf)


tagl :: (a -> b) -> a -> (b, a)
tagl f a = (f a, a)

tagr :: (a -> b) -> a -> (a, b)
tagr f a = (a, f a)

append :: [a] -> a -> [a]
append as a = as ++ [a]

(+|) :: [a] -> a -> [a]
(+|) = append

appendIfAbsent :: (Eq a) => [a] -> a -> [a]
appendIfAbsent as a | a `elem` as = as
                    | otherwise = append as a
(+|?) :: (Eq a) => [a] -> a -> [a]
(+|?) = appendIfAbsent

duplicates :: forall a. (Ord a) => [a] -> Set.Set a
duplicates as = Map.keysSet $ Map.filter (>1) theMap
  where
    count a m = Map.insertWith (const (+1)) a 1 m
    theMap :: Map.Map a Int
    theMap = foldr count mempty as

ensureUnique :: (Ord a, Show a, MonadFail m) => String -> [a] -> m [a]
ensureUnique name as =
  let dups = duplicates as in
    if not $ null $ dups
    then fail $ printf "Duplicate %s: %s" name (showItems $ Set.toList dups)
    else return as

strictZipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> Either (Int, Int) [c]
strictZipWith f = inner (0, 0)
  where
    inner _ [] [] = return []
    inner (i, j) [] bs = Left (i, j + length bs)
    inner (i, j) as [] = Left (i + length as, j)
    inner (i, j) (a:as) (b:bs) = (f a b:) <$> inner (i + 1, j + 1) as bs

strictZip :: [a] -> [b] -> Either (Int, Int) [(a, b)]
strictZip = strictZipWith (,)

fmtStrictZipError
  :: MonadFail m => String -> String -> Either (Int, Int) a -> m a
fmtStrictZipError n0 n1 = either fmt return
  where
    fmt (i, j) = fail $
      printf "Mismatched numbers of %v (%i) and %v (%i)" n0 i n1 j


partitionDifference ::
    (Ord a) => Set.Set a -> Set.Set a -> (Set.Set a, Set.Set a)
partitionDifference sa sb = (Set.difference sa sb, Set.difference sb sa)

partitionDifferenceL :: (Ord a) => [a] -> [a] -> ([a], [a])
partitionDifferenceL as bs =
  let
    (added, removed) = partitionDifference (Set.fromList as) (Set.fromList bs)
  in
    (Set.toList added, Set.toList removed)


uncamel :: String -> String
uncamel [] = []
uncamel (c:cs) = toLower c : uncamel' cs where
    uncamel' :: String -> String
    uncamel' [] = []
    uncamel' (c':cs')
        | isUpper c' = '_' : toLower c' : uncamel' cs'
        | otherwise = c' : uncamel' cs'

camel :: String -> String
camel = (foldl (++) "") . (map initCap) . (splitOn "_") where
    initCap [] = []
    initCap (c:cs) = toUpper c : cs

showItems :: (Show a) => [a] -> String
showItems = intercalate ", " . fmap show

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

bound :: forall a b m. (Enum a, Enum b, Bounded b, MonadFail m) => a -> m b
bound i =
  let
    low = fromEnum (minBound :: b)
    high = fromEnum (maxBound :: b)
    v = fromEnum i
  in
    if low <= v && v <= high
      then return $ toEnum v
      else fail "out of bounds"

-- http://stackoverflow.com/questions/2743858/safe-and-polymorphic-toenum
safeToEnum :: (MonadFail m, Enum a, Bounded a) => Int -> m a
safeToEnum i =
  let
    r = toEnum i
    theMax = maxBound `asTypeOf` r
    theMin = minBound `asTypeOf` r
  in if fromEnum theMin <= i && i <= fromEnum theMax
  then return r
  else fail "enum value out of range"
