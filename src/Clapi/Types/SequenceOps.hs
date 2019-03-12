{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
#-}

module Clapi.Types.SequenceOps
  ( SequenceOp(..), isSoAbsent
  , updateUniqList
  , DependencyOrdered, unDependencyOrdered
  , dependencyOrder, dependencyOrder'
  , fullOrderOps, fullOrderOps'
  , validateAfters
  ) where

import Prelude hiding (fail)

import Control.Lens (_1, _2, over)
import Control.Monad (foldM)
import Control.Monad.Fail (MonadFail(..))
import Data.Foldable (fold, foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Clapi.Types.AssocList (AssocList, unAssocList)
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.UniqList (UniqList, unUniqList, ulDelete, ulPresentAfter)
import Clapi.Util (mapFoldMWithKey)


data SequenceOp i
  = SoAfter (Maybe i)
  | SoAbsent
  deriving (Show, Eq, Functor, Foldable, Traversable)

isSoAbsent :: SequenceOp i -> Bool
isSoAbsent so = case so of
  SoAbsent -> True
  _ -> False

-- | Used to represent a collection of SequenceOps that have been put in the
--   correct order.
newtype DependencyOrdered k v
  = DepO
  { unDependencyOrdered :: AssocList k v
  } deriving (Show, Eq, Foldable, Semigroup, Monoid)

updateUniqList
  :: (Eq i, Ord i, Show i, MonadFail m)
  => (v -> SequenceOp i) -> DependencyOrdered i v -> UniqList i
  -> m (UniqList i)
updateUniqList f (DepO ops) ul = foldM applySo ul $ unAssocList ops
  where
    applySo acc (i, v) = case f v of
      SoAfter mi -> ulPresentAfter i mi acc
      SoAbsent -> return $ ulDelete i acc

dependencyOrder'
  :: (MonadFail m, Ord i)
  => (v -> SequenceOp i) -> Map i v -> m (DependencyOrdered i v)
dependencyOrder' f m =
    DepO . AL.unsafeMkAssocList . (absents ++) . (fmap (fmap fst))
    <$> resolveDigest snd afters
  where
    (afters, absents) = Map.foldlWithKey classify mempty m
    classify acc i v = case f v of
      SoAfter mi -> over _1 (Map.insert i (v, mi)) acc
      SoAbsent -> over _2 ((i, v):) acc


dependencyOrder
  :: (MonadFail m, Ord i)
  => Map i (SequenceOp i) -> m (DependencyOrdered i (SequenceOp i))
dependencyOrder = dependencyOrder' id


fullOrderOps'
  :: Ord i => (SequenceOp i -> v) -> UniqList i -> DependencyOrdered i v
fullOrderOps' f = DepO . go Nothing . unUniqList
  where
    go _ [] = mempty
    go prev (i:is) = AL.singleton i (f $ SoAfter prev) <> go (Just i) is

fullOrderOps :: Ord i => UniqList i -> DependencyOrdered i (SequenceOp i)
fullOrderOps = fullOrderOps' id

getChainStarts :: Ord i => (v -> Maybe i) -> Map i v -> ([(i, v)], Map i v)
getChainStarts f m =
  let
    hasUnresolvedDep = maybe False (flip Map.member m) . f
    (remainder, starts) = Map.partition hasUnresolvedDep m
  in
    (Map.toList starts, remainder)

resolveDigest :: (MonadFail m, Ord i) => (v -> Maybe i) -> Map i v -> m [(i, v)]
resolveDigest f m = if null m then return []
  else case getChainStarts f m of
    ([], _) -> fail "Unresolvable order dependencies"
    (starts, remainder) -> (starts ++) <$> resolveDigest f remainder

validateAfters :: (MonadFail m, Ord a) => Map a a -> m [[a]]
validateAfters m =
  let
    nodes = Map.keysSet m <> foldMap Set.singleton m
    start = [((a,a), [a]) | a <- Set.toList nodes]
  in
    fmap snd <$> mapFoldMWithKey f start m
  where
    f acc from to =
      let
        (start, end, rest) = foldl' g (Nothing, Nothing, []) acc
        g (is, ie, ir) x@((s, e), l)
          | s == to = (Just x, ie, ir)
          | e == from = (is, Just x, ir)
          | otherwise = (is, ie, x:ir)
      in
        case (start, end) of
          (Just ((ss, se),sl), Just ((es, ee), el)) -> return $ ((es, se), sl ++ el):rest
          _ -> fail "goaway"

-- FIXME: might want to move detect cycles to somewhere more generic so that it
-- doesn't get polluted with Valuespace-specific concerns. Also, might want to
-- make it return something more intuitive?
-- | Returns a list of arbitrary edges from the set that if removed will break
--   cycles
detectCycles :: Ord a => Set (a, a) -> [(a, a)]
detectCycles edges =
  let
    nodes = Set.mapMonotonic fst edges <> Set.map snd edges
  in
    snd $ foldl' go (Set.map Set.singleton nodes, []) edges
  where
    go :: Ord a => (Set (Set a), [(a, a)]) -> (a, a) -> (Set (Set a), [(a, a)])
    go (equivalenceSets, cycles) edge@(from, to) =
      let
        (matches, others) = Set.partition
          (\as -> from `Set.member` as || to `Set.member` as)
          equivalenceSets
      in
        case Set.size matches of
          -- We found a cycle, because `from` was already connected to `to`:
          1 -> (equivalenceSets, edge : cycles)
          -- We union the sets of connected nodes:
          2 -> (Set.singleton (fold matches) <> others, cycles)
          -- This should not happen, but is not harmful:
          _n -> (equivalenceSets, cycles)
