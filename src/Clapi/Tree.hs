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
import Control.Monad.State (State, get, put, modify, runState)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word

import Clapi.Types
  (Time, Interpolation(..), Attributee, WireValue)
import Clapi.Types.AssocList
  ( AssocList, unAssocList, alFromKeys, alFmapWithKey, alSingleton, alAlterF
  , alKeys)
import Clapi.Types.Dkmap (Dkmap)
import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.Path (
    Seg, Path, pattern Root, pattern (:/), pattern (:</),
    NodePath)
import Clapi.Types.Digests
  ( DataDigest, ChildAssignments, DataChange(..), TimeSeriesDataOp(..))
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

treePaths :: Path -> RoseTree a -> [Path]
treePaths p t = case t of
  RtEmpty -> [p]
  RtConstData _ _ -> [p]
  RtDataSeries _ -> [p]
  RtContainer al ->
    p : (mconcat $ (\(s, t') -> treePaths (p :/ s) t') <$> unAssocList al)

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


treeLookup :: Path -> RoseTree a -> Maybe (RoseTree a)
treeLookup p = getConst . treeAlterF Const p

treeDelete :: Path -> RoseTree a -> RoseTree a
treeDelete p = treeAlter (const Nothing) p

treeAdjust :: (RoseTree a -> RoseTree a) -> Path -> RoseTree a -> RoseTree a
treeAdjust f p = runIdentity . treeAdjustF (Identity . f) p

-- FIXME: can we define this in terms of treeAlterF?
treeAdjustF
  :: Functor f => (RoseTree a -> f (RoseTree a)) -> Path -> RoseTree a
  -> f (RoseTree a)
treeAdjustF f Root t = f t
treeAdjustF f (s :</ p) (RtContainer al) =
  RtContainer <$> alAlterF (fmap Just . treeAdjustF f p . maybe RtEmpty id) s al
treeAdjustF f (s :</ p) _ =
  RtContainer . alSingleton s <$> treeAdjustF f p RtEmpty

treeAlter
  :: (Maybe (RoseTree a) -> Maybe (RoseTree a)) -> Path -> RoseTree a
  -> RoseTree a
treeAlter f path = runIdentity . treeAlterF (Identity . f) path

treeAlterF
  :: forall f a. Functor f
  => (Maybe (RoseTree a) -> f (Maybe (RoseTree a))) -> Path
  -> RoseTree a -> f (RoseTree a)
treeAlterF f path tree = maybe RtEmpty id <$> inner path (Just tree)
  where
    inner :: Path -> Maybe (RoseTree a) -> f (Maybe (RoseTree a))
    inner Root mt = f mt
    inner (s :</ p) (Just t) = case t of
      RtContainer al -> Just . RtContainer <$> alAlterF (inner p) s al
      _ -> buildChildTree s p
    inner (s :</ p) Nothing = buildChildTree s p
    buildChildTree s p =
      fmap (RtContainer . alSingleton s) <$> inner p Nothing


updateTreeWithDigest
  :: ChildAssignments -> DataDigest -> RoseTree [WireValue]
  -> (Map Path [Text], RoseTree [WireValue])
updateTreeWithDigest cas dd = runState $ do
    errs <- sequence $ Map.mapWithKey applyCa cas
    errs' <- sequence $ Map.mapWithKey applyDd dd
    return $ Map.filter (not . null) $ Map.unionWith (<>) errs errs'
  where
    applyCa
      :: NodePath -> (Maybe Attributee, UniqList Seg)
      -> State (RoseTree [WireValue]) [Text]
    applyCa np (att, segs) = do
      modify $ treeAdjust (treeSetChildren segs) np
      return []
    applyDd
      :: NodePath -> DataChange
      -> State (RoseTree [WireValue]) [Text]
    applyDd np dc = case dc of
      ConstChange att wv -> do
        modify $ treeAdjust (treeConstSet att wv) np
        return []
      TimeChange m -> mconcat <$> (mapM (applyTc np) $ Map.toList m)
    applyTc
      :: NodePath
      -> (TpId, (Maybe Attributee, TimeSeriesDataOp))
      -> State (RoseTree [WireValue]) [Text]
    applyTc np (tpId, (att, op)) = case op of
      OpSet t wv i -> get >>=
        either (return . pure . Text.pack) (\vs -> put vs >> return [])
        . treeAdjustF (treeSet tpId t wv i att) np
      OpRemove -> get >>=
        either (return . pure . Text.pack) (\vs -> put vs >> return [])
        . treeAdjustF (treeRemove tpId) np

data RoseTreeNode a
  = RtnEmpty
  | RtnChildren (Maybe Attributee) (UniqList Seg)
  | RtnConstData (Maybe Attributee) a
  | RtnDataSeries (TimeSeries a)
  deriving (Show, Eq)

roseTreeNode :: RoseTree a -> RoseTreeNode a
roseTreeNode t = case t of
  RtEmpty -> RtnEmpty
  RtContainer al -> RtnChildren Nothing $ alKeys al
  RtConstData att a -> RtnConstData att a
  RtDataSeries m -> RtnDataSeries m

treeLookupNode :: Path -> RoseTree a -> Maybe (RoseTreeNode a)
treeLookupNode p = fmap roseTreeNode . treeLookup p
