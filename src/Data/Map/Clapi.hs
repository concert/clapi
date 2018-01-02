{-# OPTIONS_GHC -Wall -Wno-orphans #-}
module Data.Map.Clapi where

import qualified Data.Map as Map
import Data.Map.Strict.Merge (merge, preserveMissing, zipWithMatched)
import Data.Monoid ((<>))

import qualified Data.Maybe.Clapi as Maybe

unionM :: (Monoid a, Ord k) => Map.Map k a -> Map.Map k a -> Map.Map k a
unionM = merge preserveMissing preserveMissing (zipWithMatched (const (<>)))

lookupM :: (Monoid a, Ord k) => k -> Map.Map k a -> a
lookupM k m = Maybe.toMonoid $ Map.lookup k m

updateM :: (Monoid a, Ord k) => (a -> a) -> k -> Map.Map k a -> Map.Map k a
updateM f = Map.alter (Just . f . Maybe.toMonoid)

joinM :: (Ord k) => [Map.Map k a] -> Map.Map k [a]
joinM ms = foldl (Map.unionWith (<>)) mempty (fmap pure <$> ms)
