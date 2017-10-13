{-# LANGUAGE
    ScopedTypeVariables, TemplateHaskell, TypeFamilies
#-}
module Clapi.Tree
    -- (
    --     add, set, remove,
    --     Interpolation(..),
    --     TimePoint,
    --     TimeSeries,
    --     Node(..),
    --     ClapiTree(..),
    --     emptyTree,
    --     -- treeGet, treeAdd, treeSet, treeDelete,
    --     mapDiff, applyMapDiff, Delta(..),
    --     add', set', remove',
    --     -- getValues, getSites,
    --     failAt,
    -- )
where

import Prelude hiding (fail)
import Data.Word (Word32, Word64)
import Data.List (isPrefixOf, partition, intercalate, inits, delete)
import Data.Maybe (maybeToList, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict.Merge (
    merge, mapMissing, mapMaybeMissing, dropMissing, preserveMissing,
    zipWithMatched, zipWithMaybeMatched)
import Control.Lens (
    Lens, Lens', (.~), (&), view, over, ix, at, At(..), Index(..),
    IxValue(..), Ixed(..), makeLenses, non)
import Control.Monad (foldM)
import Control.Monad.Fail (MonadFail, fail)
import Control.Error.Util (hush)
import Text.Printf (printf)

import Clapi.Util (append, (+|), (+|?), duplicates, zipLongest, partitionDifferenceL)
import Clapi.Path (
    Name, Path, root, isChildOfAny, isChildOf, childPaths, splitBasename)
import Path.Parsing (toString)
import Clapi.Types (CanFail, Time, ClapiValue, Interpolation(..))
import Data.Maybe.Clapi (note)

import qualified Data.Maybe.Clapi as Maybe
import qualified Data.Map.Clapi as Map
import qualified Data.Map.Mos as Mos
import qualified Data.Map.Mol as Mol

type NodePath = Path
type TypePath = Path

type TimePoint a = (Interpolation, a)
type Attributee = String
type Attributed a = (Maybe Attributee, a)
type TimeSeries a = Map.Map Time (Attributed (Maybe (TimePoint a)))

type Site = String
type SiteMap a = Map.Map (Maybe Site) (TimeSeries a)

data Node a = Node {
    _getKeys :: [Name],
    _getSites :: SiteMap a}
  deriving (Eq, Show)
makeLenses ''Node

instance Monoid (Node a) where
    mempty = Node [] mempty
    mappend (Node keys m1) (Node _ m2) = Node keys (m1 <> m2)

instance Functor Node where
    -- Holy nested functors Batman!
    fmap f (Node keys m) = Node keys $ (fmap . fmap . fmap . fmap . fmap) f m

instance Foldable Node where
    foldMap f (Node _ m) = (foldMap . foldMap . foldMap . foldMap . foldMap) f m

unwrapTimePoint ::
    (MonadFail m) => (Attributed (Maybe (TimePoint a))) -> m (TimePoint a)
unwrapTimePoint = note "data deleted at time point" . snd

-- nodeGet :: (MonadFail m) => Maybe Site -> Time -> Node a -> m a
-- nodeGet site t node =
--   do
--     tp <- note "no data at time point" $
--         view (getSites . at site . non mempty . at t) node
--     unwrapTimePoint tp

getChildPaths :: NodePath -> Node a -> [NodePath]
getChildPaths rootPath node = (rootPath +|) <$> view getKeys node

type instance Index (Node a) = Maybe Site
type instance IxValue (Node a) = TimeSeries a

instance Ixed (Node a) where
    ix site = getSites . (ix site)

instance At (Node a) where
    at site = getSites . (at site)

type ClapiTree a = Map.Map NodePath (Node a)

formatTree :: (Show a) => ClapiTree a -> String
formatTree tree = intercalate "\n" lines
  where
    lines = mconcat $ ["---"] : (fmap toLines $ Map.toList tree)
    toLines (path, node) = formatPath path : nodeSiteMapToLines path node
    formatPath [] = "/"
    formatPath (n:[]) = "  " ++ n
    formatPath (n:ns) = "  " ++ formatPath ns
    pad path lines = let padding = replicate ((length path + 1) * 2) ' ' in
        fmap (padding ++) lines
    nodeSiteMapToLines path node =
        pad path $ mconcat $ fmap siteToLines $ Map.toList $ view getSites node
    siteToLines (Nothing, ts) = "global:" : (pad [] $ tsToLines ts)
    siteToLines (Just site, ts) = (site ++ ":") : (pad [] $ tsToLines ts)
    tsToLines ts = fmap attpToLine $ Map.toList ts
    attpToLine (t, (att, Nothing)) =
        printf "%s: deleted (%s)" (show t) (showAtt att)
    attpToLine (t, (att, Just (_, a))) =
        printf "%s: %s (%s)" (show t) (show a) (showAtt att)
    showAtt Nothing = "Anon"
    showAtt (Just att) = att

treeOrphansAndMissing :: ClapiTree a -> (Set.Set NodePath, Set.Set NodePath)
treeOrphansAndMissing tree = (orphans, missing)
  where
    nodes = Map.toList tree
    allChildPaths = Set.fromList . mconcat $ fmap (uncurry getChildPaths) nodes
    allChildPaths' = Set.insert root allChildPaths
    allPaths = Set.fromList $ fmap fst nodes
    orphans = Set.difference allPaths allChildPaths'
    missing = Set.difference allChildPaths allPaths

updateLookupM ::
    (MonadFail m, Ord k, Show k) => (a -> Maybe a) -> k -> Map.Map k a ->
    m (a, Map.Map k a)
updateLookupM f k m = sequenceFst $ at k updateValue m
  where
    sequenceFst (ma, b) = (\a -> (a, b)) <$> ma
    updateValue Nothing = (fail $ printf "not found %s" (show k), Nothing)
    updateValue (Just v) = (return v, f v)


-- Sets the child keys attribute of a node in the tree and adds default empty
-- nodes at the corresponding paths if required:
treeSetChildren ::
    (MonadFail m) => NodePath -> [Name] -> ClapiTree a -> m (ClapiTree a)
treeSetChildren path keys' tree
  | null $ duplicates keys' =
      do
        (node, tree') <- updateLookupM (return . (getKeys .~ keys')) path tree
        let (addedKeys, removedKeys) = partitionDifferenceL keys' (view getKeys node)
        let tree'' = foldl (flip treeInitNode) tree' ((path +|) <$> addedKeys)
        foldM (flip treeDeleteNode) tree'' ((path +|) <$> removedKeys)
  | otherwise = fail $ printf "duplicate keys %s"
        (show . Set.toList $ duplicates keys')

treeInitNode :: NodePath -> ClapiTree a -> ClapiTree a
treeInitNode p t = foldr (\(p, ns) -> Map.alter (updateNode ns) p) t pathsAndChildNames
  where
    pathsAndChildNames = zipLongest (inits p) (pure <$> p)
    updateNode ns Nothing = return $ Node ns mempty
    updateNode [n] (Just node) = return $ over getKeys (+|? n) node
    updateNode [] (Just node) = return node


treeDeleteNode ::
    forall m a. (MonadFail m) => NodePath -> ClapiTree a -> m (ClapiTree a)
treeDeleteNode p t =
  let
    tree' = fromMaybe t (pruneKey t <$> splitBasename p)
  in do
    (node, tree'') <- updateLookupM (const Nothing) p tree'
    foldM (flip treeDeleteNode) tree'' (childPaths p $ view getKeys node)
  where
    pruneKey t (p, n) = Map.update (return . over getKeys (delete n)) p t

type TreeAction m a = Maybe (Attributed (Maybe (TimePoint a))) ->
    m (Maybe (Attributed (Maybe (TimePoint a))))
treeAction ::
    (MonadFail m, Eq a) => TreeAction m a -> NodePath -> Maybe Site -> Time ->
    ClapiTree a -> m (ClapiTree a)
treeAction action path maybeSite t tree =
  let
    -- FIXME: not sure if this is the best way to do this:
    tree' = treeInitNode path tree
    node = view (at path . non mempty) tree'
  in do
    newNode <- nodeAction action maybeSite t node
    newTree <- return $ tree' & (at path) .~ (Just newNode)
    return newTree

nodeAction ::
    (MonadFail m) => TreeAction m a -> Maybe Site -> Time -> Node a ->
    m (Node a)
nodeAction action maybeSite t node =
  do
    newTimeSeries <- at t action existingTimeSeries
    return $
        node & (getSites . (at maybeSite)) .~ (Maybe.fromFoldable newTimeSeries)
  where
    existingTimeSeries = view (getSites . (ix maybeSite)) node

treeAdd ::
    forall a m. (MonadFail m, Eq a) => Maybe Attributee -> Interpolation -> a ->
    NodePath -> Maybe Site -> Time -> ClapiTree a -> m (ClapiTree a)
treeAdd att int a = treeAction add
  where
    add :: TreeAction m a
    add (Just (_, Just _)) = fail "time point already present"
    add _ = return . Just $ (att, Just (int, a))

treeSet ::
    forall a m. (MonadFail m, Eq a) => Maybe Attributee -> Interpolation -> a ->
    NodePath -> Maybe Site -> Time -> ClapiTree a -> m (ClapiTree a)
treeSet att int a path site t = treeAction set path site t
  where
    set :: TreeAction m a
    set (Just (_, Just _)) = return . Just $ (att, Just (int, a))
    set _ = fail $
        printf "missing time point at which to set %s: %s %s" (show path)
        (show t) (show site)

treeRemove ::
    forall a m. (MonadFail m, Eq a) => Maybe Attributee -> NodePath ->
    Maybe Site -> Time -> ClapiTree a -> m (ClapiTree a)
treeRemove _ path Nothing t = treeAction globalRemove path Nothing t
  where
    globalRemove :: TreeAction m a
    globalRemove (Just (_, Just _)) = return Nothing
    globalRemove _ = fail $
        printf "missing time point at which to remove %s: %s" (show path)
        (show t)
treeRemove att path justSite t = treeAction siteRemove path justSite t
  where
    siteRemove :: TreeAction m a
    siteRemove _ = return . Just $ (att, Nothing)

treeClear ::
    forall a m. (MonadFail m, Eq a) => Maybe Attributee -> NodePath ->
    Maybe Site -> Time -> ClapiTree a -> m (ClapiTree a)
treeClear _ path Nothing t tree = fail "no clearing without a site"
treeClear att path justSite t tree = treeAction clear path justSite t tree
  where
    clear :: TreeAction m a
    clear (Just _) = return Nothing
    clear _ = fail $
        printf "missing time point at which to clear %s: %s %s" (show path)
        (show t) (show justSite)


data TreeDelta a = Init TypePath
  | Delete
  | SetChildren [Name]
  | Clear Time (Maybe Site) (Maybe Attributee)
  | Remove Time (Maybe Site) (Maybe Attributee)
  | Add Time a Interpolation (Maybe Site) (Maybe Attributee)
  | Set Time a Interpolation (Maybe Site) (Maybe Attributee)
  deriving (Eq, Show)


-- treeDiff :: (Eq a) => ClapiTree a -> ClapiTree a ->
--     CanFail [(NodePath, TreeDelta a)]
-- treeDiff (ClapiTree nm1 types1) (ClapiTree nm2 types2) =
--     minimiseDeletes . minimiseClears <$> allDeltas
--   where
--     flatten :: [(a, [b])] -> [(a, b)]
--     flatten = mconcat . (fmap sequence)
--     allDeltas = flatten <$> Map.toList <$> sequence failyDeltaMap
--     failyDeltaMap = merge
--         (mapMissing onlyInNm1)
--         (mapMissing onlyInNm2)
--         (zipWithMatched inBoth) nm1 nm2
--     onlyInNm1 np n1 = Right [Delete]
--     onlyInNm2 np n2 =
--         ((maybeToList $ Init <$> Mos.getDependency np types2) ++) <$>
--         nodeDiff mempty n2
--     inBoth np n1 n2
--       | tp1 /= tp2 = ((Maybe.toMonoid $ sequence $ [Init <$> tp2]) ++) <$> nodeDiff mempty n2
--       | otherwise = nodeDiff n1 n2
--       where
--         tp1 = Mos.getDependency np types1
--         tp2 = Mos.getDependency np types2


minimiseDeletes :: [(NodePath, TreeDelta a)] -> [(NodePath, TreeDelta a)]
minimiseDeletes allDeltas = minimalDeletes ++ others
  where
    isDelete Delete = True
    isDelete _ = False
    (deletes, others) = partition (isDelete . snd) allDeltas
    minimalDeletes = snd $ foldl f ([[minBound..]], mempty) deletes
    f (state, acc) (path, Delete)
      | state `isPrefixOf` path = (state, acc)
      | otherwise = (path, (path, Delete) : acc)

minimiseClears :: [(NodePath, TreeDelta a)] -> [(NodePath, TreeDelta a)]
minimiseClears allDeltas = inits ++ minimalClears ++ others
  where
    isInit (Init _) = True
    isInit _ = False
    (inits, _others) = partition (isInit . snd) allDeltas
    isClear (Clear {}) = True
    isClear _ = False
    (clears, others) = partition (isClear . snd) _others
    initPaths = Set.fromList $ fmap fst inits
    minimalClears = Mol.toList $ Map.withoutKeys (Mol.fromList clears) initPaths


nodeDiff :: (Eq a) => Node a -> Node a -> CanFail [TreeDelta a]
nodeDiff (Node c1 sm1) (Node c2 sm2) = (setChildren ++) <$> valueDiff
  where
    setChildren
      | c1 == c2 = []
      | otherwise = [SetChildren c2]
    valueDiff = siteMapDiff sm1 sm2

siteMapDiff :: (Eq a) => SiteMap a -> SiteMap a -> CanFail [TreeDelta a]
siteMapDiff sm1 sm2 =
    mconcat <$> fmap snd <$> Map.toList <$> sequence failyMapDiff
  where
    failyMapDiff = merge
        (mapMissing onlyInSm1)
        (mapMissing onlyInSm2)
        (zipWithMatched inBoth) sm1 sm2
    onlyInSm1 site ts1 = timeSeriesDiff site ts1 mempty
    onlyInSm2 site ts2 = timeSeriesDiff site mempty ts2
    inBoth site ts1 ts2 = timeSeriesDiff site ts1 ts2

timeSeriesDiff :: (Eq a) => Maybe Site -> TimeSeries a -> TimeSeries a ->
    CanFail [TreeDelta a]
timeSeriesDiff site ts1 ts2 = fmap snd <$> Map.toList <$> sequence failyTsDiff
  where
    failyTsDiff = merge
        (mapMissing onlyInTs1)
        (mapMissing onlyInTs2)
        (zipWithMaybeMatched inBoth) ts1 ts2

    rOrC t Nothing att (Just _) = Right $ Remove t Nothing att
    rOrC t Nothing att Nothing = Left "wat"
    rOrC t site att _ = Right $ Clear t site att
    onlyInTs1 t (att1, a) = rOrC t site att1 a

    allowedR t Nothing att = Left "noo removes stored on global site"
    allowedR t site att = Right $ Remove t site att
    onlyInTs2 t (att2, Nothing) = allowedR t site att2
    onlyInTs2 t (att2, (Just (int2, a2))) = Right $ Add t a2 int2 site att2

    inBoth t (att1, Nothing) (att2, Nothing) = Just $ Left "poop"
    inBoth t (att1, Nothing) (att2, (Just (int2, a2))) =
        Just . Right $ Add t a2 int2 site att2
    inBoth t (att1, (Just (int1, a1))) (att2, Nothing) =
        Just $ allowedR t site att2
    inBoth t (att1, (Just (int1, a1))) (att2, (Just (int2, a2)))
      | int1 /= int2 || a1 /= a2 = Just . Right $ Set t a2 int2 site att2
      | otherwise = Nothing


-- treeApply :: ClapiTree a -> [(NodePath, TreeDelta a)] -> CanFail (ClapiTree a)
-- treeApply = foldM (flip f)
--   where
--     f :: (NodePath, TreeDelta a) -> ClapiTree a -> CanFail (ClapiTree a)
--     f (np, (Init tp)) = treeInitNode' np tp
--     f (np, Delete) = treeDelete np
--     f (np, (SetChildren keys)) = treeSetChildren np keys
--     f (np, (Clear t s a)) = treeClear a np s t
--     f (np, (Remove t s a)) = treeRemove a np s t
--     f (np, (Add t v i s a)) = treeAdd a i v np s t
--     f (np, (Set t v i s a)) = treeSet a i v np s t
--     treeInitNode' np tp t = return $ treeInitNode np tp t
