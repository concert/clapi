{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , TemplateHaskell
  , TypeFamilies
#-}

module Clapi.Types.Dkmap where

-- | Doubly-keyed maps.

import Prelude hiding (fail)
-- import Control.Lens (Index, IxValue, Ixed(..), At(..), makeLenses)
import Control.Monad (when)
import Control.Monad.Fail (MonadFail(..))
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map

data Dkmap k0 k1 v
  = Dkmap {_keyMap :: (Bimap k1 k0), _valueMap :: (Map k0 v)}
  deriving (Show, Eq, Functor, Foldable, Traversable)

valueMap :: Dkmap k0 k1 v -> Map k0 v
valueMap = _valueMap

empty :: (Ord k0, Ord k1) => Dkmap k0 k1 v
empty = Dkmap Bimap.empty mempty

-- | This is not idempotent
insert
  :: (MonadFail m, Ord k0, Ord k1)
  => k0 -> k1 -> v -> Dkmap k0 k1 v -> m (Dkmap k0 k1 v)
insert k0 k1 v (Dkmap km vm) = do
  when (k1 `Bimap.member` km) $ fail "Secondary key already in use"
  when (k0 `Bimap.memberR` km) $ fail "Primary key already in use"
  return $ Dkmap (Bimap.insert k1 k0 km) (Map.insert k0 v vm)

-- | This is idempotent. We only use the primary key (k0) to set the value. The
-- secondary key (k1) must not be changed to overwrite another value.
set
  :: (Ord k0, Ord k1, MonadFail m)
  => k0 -> k1 -> v -> Dkmap k0 k1 v -> m (Dkmap k0 k1 v)
set k0 k1 v (Dkmap km vm) = do
  case k1 `Bimap.lookup` km of
    Nothing -> return ()
    Just oldK0 -> when (oldK0 /= k0) $ fail "Duplicate k1"
  return $ Dkmap (Bimap.insert k1 k0 km) (Map.insert k0 v vm)

lookupK0 :: Ord k0 => k0 -> Dkmap k0 k1 v -> Maybe v
lookupK0 k0 (Dkmap _ vm) = Map.lookup k0 vm

lookupK1 :: (Ord k1, Ord k0) => k1 -> Dkmap k0 k1 v -> Maybe v
lookupK1 k1 (Dkmap km vm) = Bimap.lookup k1 km >>= flip Map.lookup vm

deleteK0 :: (Ord k0, Ord k1) => k0 -> Dkmap k0 k1 v -> Dkmap k0 k1 v
deleteK0 k0 (Dkmap km vm) = Dkmap (Bimap.deleteR k0 km) (Map.delete k0 vm)

-- | Primary key must be present in the Dkmap
deleteK0'
  :: (Ord k0, Ord k1, MonadFail m) => k0 -> Dkmap k0 k1 v -> m (Dkmap k0 k1 v)
deleteK0' k0 (Dkmap km vm) = let (mOldV, vm') = Map.alterF (,Nothing) k0 vm in
  case mOldV of
    Nothing -> fail "Primary key not in map"
    _ -> return $ Dkmap (Bimap.deleteR k0 km) vm'

deleteK1 :: (Ord k1, Ord k0) => k1 -> Dkmap k0 k1 v -> Dkmap k0 k1 v
deleteK1 k1 (Dkmap km vm) =
    Dkmap (Bimap.delete k1 km)
    (maybe vm id $ flip Map.delete vm <$> Bimap.lookup k1 km)

-- | For a given `k0` we want to change `k1`
rekey
  :: (Ord k0, Ord k1, MonadFail m)
  => k0 -> k1 -> Dkmap k0 k1 v -> m (Dkmap k0 k1 v)
rekey k0 k1' (Dkmap km vm) = do
  k1 <- maybe (fail "missing") return $ Bimap.lookupR k0 km
  return $ Dkmap (Bimap.insert k1' k0 $ Bimap.delete k1 km) vm

flatten :: (Ord k0, Ord k1) => (k1 -> v -> a) -> Dkmap k0 k1 v -> Map k0 a
flatten f (Dkmap km vm) =
  Map.mapWithKey (\k0 v -> f (fromJust $ Bimap.lookupR k0 km) v) vm


mapWithKeys
  :: (Ord k0, Ord k1) => (k0 -> k1 -> a -> b) -> Dkmap k0 k1 a -> Dkmap k0 k1 b
mapWithKeys f (Dkmap km vm) = Dkmap km $
  Map.mapWithKey (\k0 a -> f k0 (fromJust $ Bimap.lookupR k0 km) a) vm

traverseWithKeys
  :: (Applicative t, Ord k0, Ord k1)
  => (k0 -> k1 -> a -> t b) -> Dkmap k0 k1 a -> t (Dkmap k0 k1 b)
traverseWithKeys f (Dkmap km vm) = fmap (Dkmap km) $
  Map.traverseWithKey (\k0 a -> f k0 (fromJust $ Bimap.lookupR k0 km) a) vm


partitionWithKeys
  :: (Ord k0, Ord k1)
  => (k0 -> k1 -> a -> Bool) -> Dkmap k0 k1 a -> (Dkmap k0 k1 a, Dkmap k0 k1 a)
partitionWithKeys f (Dkmap km vm) =
  let
    (vm1, vm2) = Map.partitionWithKey
      (\k0 a -> f k0 (fromJust $ Bimap.lookupR k0 km) a) vm
    (km1, km2) = Bimap.partition (\_ k0 -> Map.member k0 vm1) km
  in
    (Dkmap km1 vm1, Dkmap km2 vm2)

partition
  :: (Ord k0, Ord k1)
  => (a -> Bool) -> Dkmap k0 k1 a -> (Dkmap k0 k1 a, Dkmap k0 k1 a)
partition f = partitionWithKeys (\_ _ a -> f a)
