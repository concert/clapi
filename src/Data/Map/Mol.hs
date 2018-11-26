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
singleton k a = singletonList k [a]

singletonList :: k -> [a] -> Mol k a
singletonList k as = Mol $ Map.singleton k as

fromList :: (Ord k) => [(k, a)] -> Mol k a
fromList = foldr (uncurry cons) mempty

fromSet :: (k -> [a]) -> Set k -> Mol k a
fromSet f = Mol . Map.fromSet f

toList :: (Ord k) => Mol k a -> [(k, a)]
toList (Mol m) = mconcat $ sequence <$> Map.toList m

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
