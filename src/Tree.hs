{-# LANGUAGE
    ScopedTypeVariables, TemplateHaskell, TypeFamilies
#-}
module Tree
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

import Data.Word (Word32, Word64)
import Data.List (isPrefixOf, partition, intercalate)
import Data.Maybe (maybeToList, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict.Merge (
    merge, mapMissing, mapMaybeMissing, dropMissing, preserveMissing,
    zipWithMatched, zipWithMaybeMatched)
import Control.Lens (
    Lens, Lens', (.~), (&), view, over, ix, at, At(..), Index(..),
    IxValue(..), Ixed(..), makeLenses)
import Control.Monad (foldM)
import Control.Error.Util (hush, note)
import Text.Printf (printf)

import Path (Name, Path, root, isChildOfAny)
import Path.Parsing (toString)
import Types (Time, ClapiValue, Interpolation(..))

import qualified Data.Maybe.Clapi as Maybe
import qualified Data.Map.Clapi as Map
import qualified Data.Map.Mos as Mos
import qualified Data.Map.Mol as Mol

type CanFail a = Either String a
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

append :: [a] -> a -> [a]
append as a = as ++ [a]
(+|) = append

getChildPaths :: NodePath -> Node a -> [NodePath]
getChildPaths rootPath node = childPaths childKeys
  where
    childKeys = view getKeys node
    childPaths [] = []
    childPaths (n:ns) = rootPath +| n : childPaths ns

type instance Index (Node a) = Maybe Site
type instance IxValue (Node a) = TimeSeries a

instance Ixed (Node a) where
    ix site = getSites . (ix site)

instance At (Node a) where
    at site = getSites . (at site)

data ClapiTree a = ClapiTree {
    _getNodeMap :: Map.Map NodePath (Node a),
    _getTypeMap :: Map.Map NodePath TypePath,
    _getTypeUseageMap :: Mos.Mos TypePath NodePath
    }
  deriving (Eq)
makeLenses ''ClapiTree

instance Monoid (ClapiTree a) where
    mempty = ClapiTree mempty mempty mempty
    mappend (ClapiTree nm1 tm1 tum1) (ClapiTree nm2 tm2 tum2) =
        ClapiTree (nm1 <> nm2) (tm1 <> tm2) (tum1 <> tum2)

type instance Index (ClapiTree a) = NodePath
type instance IxValue (ClapiTree a) = Node a

instance Ixed (ClapiTree a) where
    ix path = getNodeMap . (ix path)

instance At (ClapiTree a) where
    at path = getNodeMap . (at path)


formatTree :: (Show a) => ClapiTree a -> String
formatTree (ClapiTree nodeMap typeMap _) = intercalate "\n" lines
  where
    lines = mconcat $ ["---"] : (fmap toLines $ Map.toList nodeMap)
    toLines (path, node) = nodeHeader path : nodeSiteMapToLines path node
    nodeHeader path =
        printf "%s [%s]" (formatPath path)
        (toString $ Maybe.toMonoid $ Map.lookup path typeMap)
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

instance (Show a) => Show (ClapiTree a) where
    show = formatTree

treeOrphansAndMissing :: ClapiTree a -> (Set.Set NodePath, Set.Set NodePath)
treeOrphansAndMissing tree = (orphans, missing)
  where
    nodes = Map.toList $ view getNodeMap tree
    allChildPaths = Set.fromList . mconcat $ fmap (uncurry getChildPaths) nodes
    allChildPaths' = Set.insert root allChildPaths
    allPaths = Set.fromList $ fmap fst nodes
    orphans = Set.difference allPaths allChildPaths'
    missing = Set.difference allChildPaths allPaths

treeGetType :: NodePath -> ClapiTree a -> CanFail TypePath
treeGetType p (ClapiTree _ tm _) = note "Can't find type" $ Map.lookup p tm

treeInitNode :: NodePath -> TypePath -> ClapiTree a -> ClapiTree a
treeInitNode path typePath (ClapiTree nodeMap typeMap typeUsedByMap) =
    ClapiTree newNodeMap newTypeMap newTypeUsedByMap
  where
    newNodeMap = Map.insert path mempty nodeMap
    newTypeMap = Map.insert path typePath typeMap
    maybeOldTypePath = Map.lookup path typeMap
    newTypeUsedByMap =
        mosDelete' maybeOldTypePath path $
        Mos.insert typePath path $
        typeUsedByMap
    mosDelete' Nothing _ = id
    mosDelete' (Just k) a = Mos.delete k a

treeInitNodes :: Map.Map NodePath TypePath -> ClapiTree a -> ClapiTree a
treeInitNodes typePathMap (ClapiTree nm tm tum) = ClapiTree newNm newTm newTum
  where
    onlyNewNm = fmap (const mempty) typePathMap
    newNm = Map.union onlyNewNm nm
    newTm = Map.union typePathMap tm
    onlyOldTum =
      Mos.invertMap $ Map.fromList $ catMaybes $ fmap lookup $ Map.keys typePathMap
    lookup np = sequence (np, Map.lookup np tm)
    newTum = Mos.union (Mos.difference tum onlyOldTum) (Mos.invertMap typePathMap)


treeDelete :: NodePath -> ClapiTree a -> CanFail (ClapiTree a)
treeDelete path (ClapiTree nodeMap typeMap typesUsedByMap) =
    -- FIXME: this can't fail!
    Right $ ClapiTree remainingNodes newTypeMap newUsedByMap
  where
    (removedNodes, remainingNodes) = Map.partitionWithKey f nodeMap
    f perhapsChildPath _ = isPrefixOf path perhapsChildPath
    removedTypePaths = Map.mapMaybeWithKey lookupOldType removedNodes
    lookupOldType nodePath _ = Map.lookup nodePath typeMap
    newTypeMap = Map.difference typeMap removedTypePaths
    newUsedByMap = Mos.difference typesUsedByMap $ Mos.invertMap removedTypePaths

treeDeleteNodes :: [NodePath] -> ClapiTree a -> CanFail (ClapiTree a)
treeDeleteNodes nodePaths (ClapiTree nm tm tum)
  | length removedNodes < length nodePaths =
      Left $"could not delete paths " ++ (show missingPaths)
  | otherwise = Right $ ClapiTree remainingNodes newTypeMap newUsedByMap
  where
    (removedNodes, remainingNodes) =
        Map.partitionWithKey (\np _ -> isChildOfAny np nodePaths) nm
    removedTypePaths =
        Map.mapMaybeWithKey (\np _ -> Map.lookup np tm) removedNodes
    newTypeMap = Map.difference tm removedTypePaths
    newUsedByMap = Mos.difference tum $ Mos.invertMap removedTypePaths
    missingPaths =
        Set.difference (Set.fromList nodePaths) (Map.keysSet removedNodes)

treeSetChildren :: NodePath -> [Name] -> ClapiTree a -> CanFail (ClapiTree a)
treeSetChildren path keys tree = at path f tree
  where
    f Nothing = Left $ printf "not found %s" (show path)
    f (Just node) = Right . Just $ over getKeys (const keys) node

treeSetChildren' :: Map.Map NodePath [Name] -> ClapiTree a ->
    CanFail (ClapiTree a)
treeSetChildren' childKeyMap tree@(ClapiTree nm _ _) =
  do
    changedNodes <- sequence $ merge
        (mapMissing notFound)
        dropMissing
        (zipWithMatched changeChildren) childKeyMap nm
    return $ tree & getNodeMap .~ Map.union changedNodes nm
  where
    notFound np _ = Left $ printf "not found %s" (show np)
    changeChildren _ children node = Right $ node & getKeys .~ children

type TreeAction a = Maybe (Attributed (Maybe (TimePoint a))) ->
    CanFail (Maybe (Attributed (Maybe (TimePoint a))))
treeAction :: TreeAction a -> NodePath -> Maybe Site -> Time -> ClapiTree a ->
    CanFail (ClapiTree a)
treeAction action path maybeSite t tree =
  do
    node <- (note $ printf "not found %s" (show path)) $ view (at path) tree
    newNode <- nodeAction action maybeSite t node
    newTree <- return $ tree & (at path) .~ (Just newNode)
    return newTree

nodeAction :: TreeAction a -> Maybe Site -> Time -> Node a -> CanFail (Node a)
nodeAction action maybeSite t node =
  do
    newTimeSeries <- at t action existingTimeSeries
    return $
        node & (getSites . (at maybeSite)) .~ (Maybe.fromFoldable newTimeSeries)
  where
    existingTimeSeries = view (getSites . (ix maybeSite)) node

treeAdd :: forall a. Maybe Attributee -> Interpolation -> a -> NodePath ->
    Maybe Site -> Time -> ClapiTree a -> CanFail (ClapiTree a)
treeAdd att int a = treeAction add
  where
    add :: TreeAction a
    add (Just (_, Just _)) = Left "time point already present"
    add _ = Right . Just $ (att, Just (int, a))

treeSet :: forall a. Maybe Attributee -> Interpolation -> a -> NodePath ->
    Maybe Site -> Time -> ClapiTree a -> CanFail (ClapiTree a)
treeSet att int a path site t = treeAction set path site t
  where
    set :: TreeAction a
    set (Just (_, Just _)) = Right . Just $ (att, Just (int, a))
    set _ = Left $
        printf "missing time point at which to set %s: %s %s" (show path)
        (show t) (show site)

treeRemove :: forall a. Maybe Attributee -> NodePath -> Maybe Site ->
    Time -> ClapiTree a -> CanFail (ClapiTree a)
treeRemove _ path Nothing t = treeAction globalRemove path Nothing t
  where
    globalRemove :: TreeAction a
    globalRemove (Just (_, Just _)) = Right Nothing
    globalRemove _ = Left $
        printf "missing time point at which to remove %s: %s" (show path)
        (show t)
treeRemove att path justSite t = treeAction siteRemove path justSite t
  where
    siteRemove :: TreeAction a
    siteRemove _ = Right . Just $ (att, Nothing)

treeClear :: forall a. Maybe Attributee -> NodePath -> Maybe Site -> Time ->
    ClapiTree a -> CanFail (ClapiTree a)
treeClear _ path Nothing t tree = Left "no clearing without a site"
treeClear att path justSite t tree = treeAction clear path justSite t tree
  where
    clear :: TreeAction a
    clear (Just _) = Right Nothing
    clear _ = Left $
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


treeDiff :: (Eq a) => ClapiTree a -> ClapiTree a ->
    CanFail [(NodePath, TreeDelta a)]
treeDiff (ClapiTree nm1 tm1 tum1) (ClapiTree nm2 tm2 tum2) =
    minimiseDeletes . minimiseClears <$> allDeltas
  where
    flatten :: [(a, [b])] -> [(a, b)]
    flatten = mconcat . (fmap sequence)
    allDeltas = flatten <$> Map.toList <$> sequence failyDeltaMap
    failyDeltaMap = merge
        (mapMissing onlyInNm1)
        (mapMissing onlyInNm2)
        (zipWithMatched inBoth) nm1 nm2
    onlyInNm1 np n1 = Right [Delete]
    onlyInNm2 np n2 =
        ((maybeToList $ Init <$> Map.lookup np tm2) ++) <$>
        nodeDiff mempty n2
    inBoth np n1 n2
      | tp1 /= tp2 = ((Maybe.toMonoid $ sequence $ [Init <$> tp2]) ++) <$> nodeDiff mempty n2
      | otherwise = nodeDiff n1 n2
      where
        tp1 = Map.lookup np tm1
        tp2 = Map.lookup np tm2


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


treeApply :: ClapiTree a -> [(NodePath, TreeDelta a)] -> CanFail (ClapiTree a)
treeApply = foldM (flip f)
  where
    f :: (NodePath, TreeDelta a) -> ClapiTree a -> CanFail (ClapiTree a)
    f (np, (Init tp)) = treeInitNode' np tp
    f (np, Delete) = treeDelete np
    f (np, (SetChildren keys)) = treeSetChildren np keys
    f (np, (Clear t s a)) = treeClear a np s t
    f (np, (Remove t s a)) = treeRemove a np s t
    f (np, (Add t v i s a)) = treeAdd a i v np s t
    f (np, (Set t v i s a)) = treeSet a i v np s t
    treeInitNode' np tp t = return $ treeInitNode np tp t
