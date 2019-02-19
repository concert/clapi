{-# LANGUAGE
    DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , PolyKinds
  , TypeOperators
#-}

module Clapi.Util
  ( duplicates, ensureUnique
  , strictZipWith, strictZip, fmtStrictZipError
  , partitionDifference, partitionDifferenceF
  , camel
  , uncamel
  , showItems
  , bound
  , safeToEnum
  , foldMapM
  , mapPartitionEither, mapFoldMWithKey, mapFoldMapMWithKey
  , nestMapsByKey
  , flattenNestedMaps, foldlNestedMaps
  , liftRefl, pairRefl
  , Mappable(..)
  , Filterable(..), filterMaybe, justs, lefts, rights
  ) where

import Prelude hiding (fail, filter, map)

import Control.Monad (foldM)
import Control.Monad.Fail (MonadFail, fail)
import Data.Char (isUpper, toLower, toUpper)
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.Foldable (Foldable, toList)
import qualified Data.Foldable as Foldable
import Data.List (intercalate)
import qualified Data.List as List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality ((:~:)(..))
import Text.Printf (printf)


duplicates :: forall f a. (Foldable f, Ord a) => f a -> Set a
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


partitionDifference
  :: (Ord a) => Set.Set a -> Set.Set a -> (Set.Set a, Set.Set a)
partitionDifference as bs = (Set.difference as bs, Set.difference bs as)

partitionDifferenceF
  :: (Ord a, Foldable f, Foldable g, Applicative m, Monoid (m a))
  => f a -> g a -> (m a, m a)
partitionDifferenceF as bs =
  let
    (l, r) = partitionDifference (toSet as) (toSet bs)
  in
    (fromSet l, fromSet r)
  where
    toSet :: (Foldable f, Ord a) => f a -> Set a
    toSet = Set.fromList . Foldable.toList
    fromSet = foldMap pure

uncamel :: String -> String
uncamel [] = []
uncamel (c:cs) = toLower c : uncamel' cs where
    uncamel' :: String -> String
    uncamel' [] = []
    uncamel' (c':cs')
        | isUpper c' = '_' : toLower c' : uncamel' cs'
        | otherwise = c' : uncamel' cs'

camel :: String -> String
camel = (foldl (++) "") . (fmap initCap) . (splitOn "_") where
    initCap [] = []
    initCap (c:cs) = toUpper c : cs

showItems :: (Foldable f, Show a) => f a -> String
showItems = intercalate ", " . fmap show . toList

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

nestMapsByKey
  :: (Ord k, Ord k0, Ord k1)
  => (k -> Maybe (k0, k1)) -> Map k a -> (Map k a, Map k0 (Map k1 a))
nestMapsByKey f = Map.foldlWithKey g mempty
  where
    g (unsplit, nested) k val = case f k of
      Just (k0, k1) ->
        ( unsplit
        , Map.alter (Just . Map.insert k1 val . maybe mempty id) k0 nested)
      Nothing -> (Map.insert k val unsplit, nested)

flattenNestedMaps
  :: (Ord k0, Ord k1, Ord k2)
  => (k0 -> k1 -> k2) -> Map k0 (Map k1 v) -> Map k2 v
flattenNestedMaps f = Map.foldlWithKey inner mempty
  where
    inner acc k0 m = Map.union acc $ Map.mapKeys (\k1 -> f k0 k1) m

foldlNestedMaps
  :: (acc -> k0 -> k1 -> v -> acc) -> acc -> Map k0 (Map k1 v) -> acc
foldlNestedMaps f = Map.foldlWithKey g
  where
    g acc k0 = Map.foldlWithKey (\acc' k1 v -> f acc' k0 k1 v) acc

foldMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldMapM f = foldM (\b a -> (b <>) <$> f a) mempty

mapPartitionEither :: Map k (Either a b) -> (Map k a, Map k b)
mapPartitionEither m = let (ls, rs) = Map.partition isLeft m in
  (fromLeft undefined <$> ls, fromRight undefined <$> rs)

mapFoldMWithKey
  :: forall k a b m. Monad m => (b -> k -> a -> m b) -> b -> Map k a -> m b
mapFoldMWithKey f b m = Map.foldrWithKey f' return m b
  where
    f' :: k -> a -> (b -> m b) -> b -> m b
    f' k a fm b' = f b' k a >>= fm

mapFoldMapMWithKey
  :: forall k a b m. (Monoid b, Monad m) => (k -> a -> m b) -> Map k a -> m b
mapFoldMapMWithKey f = mapFoldMWithKey f' mempty
  where
    f' :: b -> k -> a -> m b
    f' b k a = (b <>) <$> f k a

liftRefl :: a :~: b -> f a :~: f b
liftRefl Refl = Refl

pairRefl :: a :~: b -> c :~: d -> (a, c) :~: (b, d)
pairRefl Refl Refl = Refl

class Mappable t a b where
  map :: (a -> b) -> t a -> t b
  default map :: Functor t => (a -> b) -> t a -> t b
  map = fmap

instance {-# OVERLAPPABLE #-} Functor t => Mappable t a b
instance Ord b => Mappable Set a b where
  map = Set.map

class Filterable t where
  filter :: (a -> Bool) -> t a -> t a

instance Filterable [] where
  filter = List.filter
instance Filterable (Map k) where
  filter = Map.filter
instance Filterable Set where
  filter = Set.filter

filterMaybe
  :: (Filterable t, Mappable t a (Maybe b), Mappable t (Maybe b) b)
  => (a -> Maybe b) -> t a -> t b
filterMaybe f = justs . map f

justs
  :: (Filterable t, Mappable t (Maybe a) a)
  => t (Maybe a) -> t a
justs = map fromJust . filter isJust

-- | A more polymorphic version of Either.lefts
lefts :: (Filterable t, Mappable t (Either a b) a) => t (Either a b) -> t a
lefts = map (fromLeft $ error "impossible") . filter isLeft

-- | A more polymorphic version of Either.rights
rights :: (Filterable t, Mappable t (Either a b) b)  => t (Either a b) -> t b
rights = map (fromRight $ error "impossible") . filter isRight
