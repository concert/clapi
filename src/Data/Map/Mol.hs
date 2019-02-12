{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
#-}
module Data.Map.Mol where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

import Data.Map.Clapi as Map

newtype Mol k a
  = Mol {unMol :: Map k [a]} deriving (Show, Eq, Ord, Functor, Foldable)

instance Ord k => Semigroup (Mol k a) where
  (<>) = union

instance Ord k => Monoid (Mol k a) where
  mempty = Mol mempty

singleton :: k -> a -> Mol k a
singleton k a = Mol $ Map.singleton k [a]

singletonList :: k -> [a] -> Mol k a
singletonList k as = Mol $ if null as
  then Map.empty
  else Map.singleton k as

fromList :: (Ord k) => [(k, a)] -> Mol k a
fromList = foldr (uncurry cons) mempty

fromSet :: (k -> [a]) -> Set k -> Mol k a
fromSet f = Mol . Map.filter (not . null) . Map.fromSet f

toList :: (Ord k) => Mol k a -> [(k, a)]
toList (Mol m) = mconcat $ sequence <$> Map.toList m

fromMap :: Map k [a] -> Mol k a
fromMap = Mol . Map.filter (not . null)

fromSetSingle :: (k -> a) -> Set k -> Mol k a
fromSetSingle f = fromSet (pure . f)

keys :: Mol k a -> [k]
keys = Map.keys . unMol

keysSet :: Mol k a -> Set k
keysSet = Map.keysSet . unMol

lookup :: (Ord k) => k -> Mol k a -> [a]
lookup k = maybe [] id . Map.lookup k . unMol

cons :: (Ord k) => k -> a -> Mol k a -> Mol k a
cons k a = Mol . Map.updateM (a :) k . unMol

append :: (Ord k) => k -> a -> Mol k a -> Mol k a
append k a = extend k [a]

extend :: (Ord k) => k -> [a] -> Mol k a -> Mol k a
extend k as = Mol . Map.updateM (++ as) k . unMol

prepend :: (Ord k) => k -> [a] -> Mol k a -> Mol k a
prepend k as = Mol . Map.updateM (as ++) k . unMol

union :: (Ord k) => Mol k a -> Mol k a -> Mol k a
union (Mol m1) (Mol m2) = Mol $ Map.unionWith (<>) m1 m2

unions :: (Ord k) => [Mol k a] -> Mol k a
unions = Mol . Map.unionsWith (<>) . fmap unMol

mapKeys :: Ord k2 => (k1 -> k2) -> Mol k1 a -> Mol k2 a
mapKeys f = Mol . Map.mapKeys f . unMol

mapKeysMonotonic :: (k1 -> k2) -> Mol k1 a -> Mol k2 a
mapKeysMonotonic f = Mol . Map.mapKeysMonotonic f . unMol

filterWithKey :: (k -> a -> Bool) -> Mol k a -> Mol k a
filterWithKey p = fromMap . Map.mapWithKey (\k as -> filter (p k) as) . unMol
