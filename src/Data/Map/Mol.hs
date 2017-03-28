module Data.Map.Mol where

import qualified Data.Map as Map

import Data.Map.Clapi as Map

type Mol k a = Map.Map k [a]

fromList :: (Ord k) => [(k, a)] -> Mol k a
fromList = foldr (uncurry cons) mempty

toList :: (Ord k) => Mol k a -> [(k, a)]
toList mol = mconcat $ sequence <$> Map.toList mol

cons :: (Ord k) => k -> a -> Mol k a -> Mol k a
cons k a = Map.updateM (a :) k

append :: (Ord k) => k -> a -> Mol k a -> Mol k a
append k a = extend k [a]

extend :: (Ord k) => k -> [a] -> Mol k a -> Mol k a
extend k as = Map.updateM (++ as) k

prepend :: (Ord k) => k -> [a] -> Mol k a -> Mol k a
prepend k as = Map.updateM (as ++) k
