module Data.Map.Clapi where

import qualified Data.Map as Map

import qualified Data.Maybe.Clapi as Maybe

lookupM :: (Monoid a, Ord k) => k -> Map.Map k a -> a
lookupM k m = Maybe.toMonoid $ Map.lookup k m

updateM :: (Monoid a, Ord k) => (a -> a) -> k -> Map.Map k a -> Map.Map k a
updateM f = Map.alter (Just . f . Maybe.toMonoid)
