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
  , extractDependencyChains, DependencyError(..)
  ) where

import Prelude hiding (fail)

import Control.Lens (_1, _2, _3, over, set)
import Control.Monad (foldM, unless)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Map.Mos as Mos

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

data DependencyError i
  = DuplicateReferences (Map i (Set i))
  | CyclicReferences [[(i, i)]]
  deriving (Show, Eq)

extractDependencyChains
  :: forall i v m. (MonadError (DependencyError i) m, Ord i)
  => (v -> i) -> Map i v -> m [[(i, v)]]
extractDependencyChains proj m =
  let
    dupRefs = detectDuplicates $ proj <$> m
    referers = Map.keysSet m
    referees = foldMap Set.singleton $ proj <$> m
    onlyReferees = referees `Set.difference` referers
    initChains =
         [((i, i), [(i, v)]) | (i, v) <- Map.toList m]
      ++ [((i, i), []) | i <- Set.toList onlyReferees]
    (chains, cycles) = runWriter $ mapFoldMWithKey link initChains m
  in do
    unless (null dupRefs) $ throwError $ DuplicateReferences dupRefs
    unless (null cycles) $ throwError $ CyclicReferences cycles
    return $ snd <$> chains
  where
    link
      :: Eq i
      => [((i, i), [(i, v)])] -> i -> v
      -> Writer [[(i, i)]] [((i, i), [(i, v)])]
    link chains referer referee =
      let
        -- Find the two chains that are joined by the current edge by looking
        -- at the chains start and end nodes:
        (chainA, chainB, rest) =
          foldl' findChains (Nothing, Nothing, []) chains
        findChains acc x@((start, end), _)
          | start == proj referee = set _1 (Just x) acc
          | end == referer = set _2 (Just x) acc
          | otherwise = over _3 (x:) acc
      in
        case (chainA, chainB) of
          -- Join two chains:
          (Just ((_, end), itemsA), Just ((start, _), itemsB)) ->
            return $ ((start, end), itemsA ++ itemsB):rest
          -- Attempt to form a loop from end to start (NB: which one of the
          -- pair is Nothing depends on the order of the guards above):
          (Just (_, l), Nothing) -> tell [fmap proj <$> l] >> return chains
          -- We can't have a loop that start halfway through a chain because
          -- we started from a map (this would mean duplicate keys).
          -- We can't have a loop that ends halfway through a chain because
          -- dupRefs picks that up:
          _ -> error "Impossible"

detectDuplicates :: (Ord k, Ord v) => Map k v -> Map v (Set k)
detectDuplicates =  Map.filter ((>= 2) . Set.size) . Mos.unMos . Mos.invertMap
