{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Clapi.Tree where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.State (StateT, get, put, modify, execStateT)
import Data.Functor.Identity (Identity(..))
import qualified Data.Map.Strict as Map
import Data.Word

import Clapi.Types
  (Time, Interpolation(..), Attributee, Site, WireValue)
import Clapi.Types.AssocList
  (AssocList, unAssocList, alFromKeys, alFmapWithKey, alSingleton, alAlterF)
import Clapi.Types.Dkmap (Dkmap)
import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.Path (
    Seg, Path, pattern Root, pattern (:/), pattern (:</),
    NodePath, TypePath)
import Clapi.Types.Messages
  ( DataDigest(..), ChildAssignments, DataChange(..), ConstDataOp(..)
  , TimeSeriesDataOp(..))
import Clapi.Types.UniqList (UniqList)

type TpId = Word32

type TimePoint a = (Interpolation, a)
type Attributed a = (Maybe Attributee, a)
type TimeSeries a = Dkmap TpId Time (Attributed (TimePoint a))

data RoseTree a
  = RtEmpty
  | RtContainer (AssocList Seg (RoseTree a))
  | RtConstData (Maybe Attributee) a
  | RtDataSeries (TimeSeries a)
  deriving (Show, Eq, Functor, Foldable)

treeMissing :: RoseTree a -> [Path]
treeMissing = inner Root
  where
    inner p RtEmpty = [p]
    inner p (RtContainer al) =
      mconcat $ (\(s, rt) -> inner (p :/ s) rt) <$> unAssocList al
    inner _ _ = []

treeSetChildren :: UniqList Seg -> RoseTree a -> RoseTree a
treeSetChildren ss (RtContainer children) =
  let
    kids = Map.fromList $ unAssocList children
    children' = alFmapWithKey
        (\s _ -> maybe RtEmpty id $ Map.lookup s kids) (alFromKeys () ss)
  in
    RtContainer children'
treeSetChildren ss _ = RtContainer (alFromKeys RtEmpty ss)

treeConstSet :: Maybe Attributee -> a -> RoseTree a -> RoseTree a
treeConstSet att a _ = RtConstData att a

treeSet
  :: MonadFail m => TpId -> Time -> a -> Interpolation -> Maybe Attributee
  -> RoseTree a -> m (RoseTree a)
treeSet tpId t a i att rt =
  let
    ts' = case rt of
      RtDataSeries ts -> ts
      _ -> Dkmap.empty
  in
    RtDataSeries <$> Dkmap.set tpId t (att, (i, a)) ts'


treeRemove :: MonadFail m => TpId -> RoseTree a -> m (RoseTree a)
treeRemove tpId rt = case rt of
  RtDataSeries ts -> RtDataSeries <$> Dkmap.deleteK0' tpId ts
  _ -> fail "Not a time series"


treeAdjust :: (RoseTree a -> RoseTree a) -> Path -> RoseTree a -> RoseTree a
treeAdjust f p = runIdentity . treeAdjustF (Identity . f) p

treeAdjustF
  :: Functor f => (RoseTree a -> f (RoseTree a)) -> Path -> RoseTree a
  -> f (RoseTree a)
treeAdjustF f Root t = f t
treeAdjustF f (s :</ p) (RtContainer al) =
  RtContainer <$> alAlterF (fmap Just . treeAdjustF f p . maybe RtEmpty id) s al
treeAdjustF f (s :</ p) _ =
  RtContainer . alSingleton s <$> treeAdjustF f p RtEmpty


updateTreeWithDigest
  :: MonadFail m
  => ChildAssignments -> DataDigest -> RoseTree [WireValue]
  -> m (RoseTree [WireValue])
updateTreeWithDigest cas (DataDigest m) = execStateT $
    mapM applyCa (Map.toList cas) >> mapM applyDd (Map.toList m)
  where
    applyCa
      :: MonadFail m => (NodePath, (Maybe Attributee, UniqList Seg))
      -> StateT (RoseTree [WireValue]) m ()
    applyCa (np, (att, segs)) = modify $ treeAdjust (treeSetChildren segs) np
    applyDd
      :: MonadFail m => ((NodePath, Maybe Site), DataChange)
      -> StateT (RoseTree [WireValue]) m ()
    applyDd ((_, Just _), dc) = fail "Sites not implemented"
    applyDd ((np, Nothing), dc) = case dc of
      ConstChange (att, OpConstSet wv) ->
        modify (treeAdjust (treeConstSet att wv) np)
      ConstChange _ -> fail "Site clear: sites not implemented"
      TimeChange m -> mapM_ (applyTc np) $ Map.toList m
    applyTc
      :: MonadFail m => NodePath
      -> (TpId, (Maybe Attributee, TimeSeriesDataOp))
      -> StateT (RoseTree [WireValue]) m ()
    applyTc np (tpId, (att, op)) = case op of
      OpSet t wv i -> get >>= treeAdjustF (treeSet tpId t wv i att) np >>= put
      OpRemove -> get >>= treeAdjustF (treeRemove tpId) np >>= put
      OpClear -> fail "Tp clear: sites not implemented"
