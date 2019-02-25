{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , LambdaCase
#-}

module Clapi.Tree where

import Prelude hiding (fail, lookup)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.State (State, get, put, modify, runState, state)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map.Mol (Mol)
import qualified Data.Map.Mol as Mol

import Clapi.Types
  (Time, Interpolation(..), Attributee, SomeWireValue)
import Clapi.Types.AssocList (AssocList(..))
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Dkmap (Dkmap)
import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.Path (
    Seg, Path, pattern Root, pattern (:/), pattern (:</))
import Clapi.Types.Digests
  ( DataDigest, ContOps, DataChange(..), TimeSeriesDataOp(..), TpId)
import Clapi.Types.SequenceOps (SequenceOp, updateUniqList)

type TimePoint a = (Interpolation, a)
type Attributed a = (Maybe Attributee, a)
type TimeSeries a = Dkmap TpId Time (Attributed (TimePoint a))

data RoseTree a
  = RtEmpty
  | RtContainer (AssocList Seg (Maybe Attributee, RoseTree a))
  | RtConstData (Maybe Attributee) a
  | RtDataSeries (TimeSeries a)
  deriving (Show, Eq, Functor, Foldable)

missing :: RoseTree a -> [Path]
missing = inner Root
  where
    inner p RtEmpty = [p]
    inner p (RtContainer al) =
      mconcat $ (\(s, (_, rt)) -> inner (p :/ s) rt) <$> unAssocList al
    inner _ _ = []

children :: RoseTree a -> AssocList Seg (RoseTree a)
children t = case t of
    RtContainer al -> snd <$> al
    _ -> AL.empty

childNames :: RoseTree a -> [Seg]
childNames = fmap fst . unAssocList . children

-- FIXME: define in terms of treeChildren (if even used)
paths :: Path -> RoseTree a -> [Path]
paths p t = case t of
  RtEmpty -> [p]
  RtConstData _ _ -> [p]
  RtDataSeries _ -> [p]
  RtContainer al ->
    p : (mconcat $ (\(s, (_, t')) -> paths (p :/ s) t') <$> unAssocList al)

applyReorderings
  :: MonadFail m
  => Map Seg (Maybe Attributee, SequenceOp Seg) -> RoseTree a
  -> m (RoseTree a)
applyReorderings contOps (RtContainer kids) =
  let
    attMap = fst <$> contOps
    childMap = Map.foldlWithKey'
        (\acc k att -> Map.alter (Just . maybe (att, RtEmpty) id) k acc)
        (AL.toMap kids)
        (fst <$> contOps)
    reattribute s (oldMa, rt) = (Map.findWithDefault oldMa s attMap, rt)
  in
    RtContainer . AL.fmapWithKey reattribute . AL.pickFromMap childMap
    <$> (updateUniqList (snd <$> contOps) $ AL.keys kids)
applyReorderings contOps RtEmpty = applyReorderings contOps (RtContainer AL.empty)
applyReorderings _ _ = fail "Not a container"

constSet :: Maybe Attributee -> a -> RoseTree a -> RoseTree a
constSet att a _ = RtConstData att a

set
  :: MonadFail m => TpId -> Time -> a -> Interpolation -> Maybe Attributee
  -> RoseTree a -> m (RoseTree a)
set tpId t a i att rt =
  let
    ts' = case rt of
      RtDataSeries ts -> ts
      _ -> Dkmap.empty
  in
    RtDataSeries <$> Dkmap.set tpId t (att, (i, a)) ts'


remove :: MonadFail m => TpId -> RoseTree a -> m (RoseTree a)
remove tpId rt = case rt of
  RtDataSeries ts -> RtDataSeries <$> Dkmap.deleteK0' tpId ts
  _ -> fail "Not a time series"


lookup :: Path -> RoseTree a -> Maybe (RoseTree a)
lookup p = getConst . alterF Nothing Const p

insert :: Maybe Attributee -> Path -> RoseTree a -> RoseTree a -> RoseTree a
insert att p t = alter att (const $ Just t) p

delete :: Path -> RoseTree a -> RoseTree a
delete p = alter Nothing (const Nothing) p

adjust
  :: Maybe Attributee -> (RoseTree a -> RoseTree a) -> Path -> RoseTree a
  -> RoseTree a
adjust att f p = runIdentity . adjustF att (Identity . f) p

adjustF
  :: Functor f => Maybe Attributee -> (RoseTree a -> f (RoseTree a)) -> Path
  -> RoseTree a -> f (RoseTree a)
adjustF att f = alterF att (fmap Just . f . maybe RtEmpty id)

alter
  :: Maybe Attributee -> (Maybe (RoseTree a) -> Maybe (RoseTree a)) -> Path
  -> RoseTree a -> RoseTree a
alter att f path = runIdentity . alterF att (Identity . f) path

alterF
  :: forall f a. Functor f
  => Maybe Attributee -> (Maybe (RoseTree a) -> f (Maybe (RoseTree a))) -> Path
  -> RoseTree a -> f (RoseTree a)
alterF att f path tree = maybe tree snd <$> inner path (Just (att, tree))
  where
    inner
      :: Path -> Maybe (Maybe Attributee, RoseTree a)
      -> f (Maybe (Maybe Attributee, RoseTree a))
    inner Root mat = fmap (att,) <$> f (snd <$> mat)
    inner (s :</ p) existingChild@(Just (att', t)) = case t of
      RtContainer al -> Just . (att',) . RtContainer <$> AL.alterF (inner p) s al
      _ -> maybe existingChild Just <$> buildChildTree s p
    inner (s :</ p) Nothing = buildChildTree s p
    buildChildTree s p =
      fmap ((att,) . RtContainer . AL.singleton s) <$> inner p Nothing

updateTreeStructure
  :: ContOps Seg -> RoseTree a -> (Mol Path Text, RoseTree a)
updateTreeStructure contOps = runState $ do
    errs <- sequence $ Map.mapWithKey applyContOp contOps
    return $ Mol.fromMap errs
  where
    applyContOp
      :: Path -> Map Seg (Maybe Attributee, SequenceOp Seg)
      -> State (RoseTree a) [Text]
    applyContOp np m = do
      eRt <- adjustF Nothing (applyReorderings m) np <$> get
      either (return . pure . Text.pack) (\rt -> put rt >> return []) eRt

updateTreeData
  :: DataDigest -> RoseTree [SomeWireValue]
  -> (Mol Path Text, RoseTree [SomeWireValue])
updateTreeData dd = runState $ do
    errs <- AL.toMap <$> (sequence $ AL.fmapWithKey applyDd dd)
    return $ Mol.fromMap errs
  where
    applyDd
      :: Path -> DataChange
      -> State (RoseTree [SomeWireValue]) [Text]
    applyDd np dc = case dc of
      ConstChange att wv -> do
        modify $ adjust Nothing (constSet att wv) np
        return []
      TimeChange m -> mconcat <$> (mapM (applyTc np) $ Map.toList m)
    applyTc
      :: Path
      -> (TpId, (Maybe Attributee, TimeSeriesDataOp))
      -> State (RoseTree [SomeWireValue]) [Text]
    applyTc np (tpId, (att, op)) = case op of
      OpSet t wv i -> get >>=
        either (return . pure . Text.pack) (\vs -> put vs >> return [])
        . adjustF Nothing (set tpId t wv i att) np
      OpRemove -> get >>=
        either (return . pure . Text.pack) (\vs -> put vs >> return [])
        . adjustF Nothing (remove tpId) np


-- FIXME: Maybe reorder the args to reflect application order?
updateTreeWithDigest
  :: ContOps Seg -> DataDigest -> RoseTree [SomeWireValue]
  -> (Mol Path Text, RoseTree [SomeWireValue])
updateTreeWithDigest contOps dd = runState $ do
    errs <- state (updateTreeData dd)
    errs' <- state (updateTreeStructure contOps)
    return $ errs <> errs'

data RoseTreeNode a
  = RtnEmpty
  | RtnChildren (AssocList Seg (Maybe Attributee))
  | RtnConstData (Maybe Attributee) a
  | RtnDataSeries (TimeSeries a)
  deriving (Show, Eq)

-- FIXME: perhaps this should return Maybe (RoseTreeNode a), and the RtEmpty
-- case return Nothing, because the only place we use RtnEmpty we don't actually
-- want a node!
roseTreeNode :: RoseTree a -> RoseTreeNode a
roseTreeNode t = case t of
  RtEmpty -> RtnEmpty
  RtContainer al -> RtnChildren $ fmap fst al
  RtConstData att a -> RtnConstData att a
  RtDataSeries m -> RtnDataSeries m

lookupNode :: Path -> RoseTree a -> Maybe (RoseTreeNode a)
lookupNode p = fmap roseTreeNode . lookup p

data RoseTreeNodeType
  = RtntEmpty
  | RtntContainer
  | RtntConstData
  | RtntDataSeries
  deriving Show

rtType :: RoseTree a -> RoseTreeNodeType
rtType rt = case rt of
  RtEmpty -> RtntEmpty
  RtContainer _ -> RtntContainer
  RtConstData _ _ -> RtntConstData
  RtDataSeries _ -> RtntDataSeries


constSetAt :: Maybe Attributee -> Path -> a -> RoseTree a -> RoseTree a
constSetAt att p a = adjust att (constSet att a) p

setTpAt
  :: MonadFail m
  => Maybe Attributee -> Path -> TpId -> Time -> a -> Interpolation
  -> RoseTree a -> m (RoseTree a)
setTpAt att p tpid t a i = adjustF Nothing (set tpid t a i att) p

removeTpAt
  :: MonadFail m
  => Maybe Attributee -> Path -> TpId -> RoseTree a -> m (RoseTree a)
removeTpAt att p tpid = adjustF Nothing (remove tpid) p

applyReorderingsAt
  :: MonadFail m
  => Path -> Map Seg (Maybe Attributee, SequenceOp Seg) -> RoseTree a
  -> m (RoseTree a)
applyReorderingsAt p cOps = adjustF Nothing (applyReorderings cOps) p

childNamesAt :: Path -> RoseTree a -> Maybe [Seg]
childNamesAt p = fmap childNames . lookup p

-- | Initialise a container at the given path if it is not already a container.
initContainerAt :: Path -> RoseTree a -> RoseTree a
initContainerAt = alter Nothing $ Just . maybe (RtContainer mempty)
  (\case
    t@(RtContainer {}) -> t
    _ -> RtContainer mempty
  )
