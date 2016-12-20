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
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict.Merge (
    merge, mapMissing, dropMissing, preserveMissing, zipWithMatched,
    zipWithMaybeMatched)
import Control.Lens (
    Lens, Lens', (.~), (&), view, over, ix, at, At(..), Index(..),
    IxValue(..), Ixed(..), makeLenses)
import Control.Error.Util (hush, note)
import Control.Applicative (Const(..))
import Text.Printf (printf)

import Parsing (pathToString)
import Types (Name, ClapiPath(..), root, up, initLast, Time, ClapiValue)


type CanFail a = Either String a
type NodePath = ClapiPath
type TypePath = ClapiPath

maybeToMonoid :: (Monoid a) => Maybe a -> a
maybeToMonoid Nothing = mempty
maybeToMonoid (Just a) = a

foldableToMaybe :: (Foldable t) => t a -> Maybe (t a)
foldableToMaybe t
  | null t = Nothing
  | otherwise = Just t

mapLookupM :: (Monoid a, Ord k) => k -> Map.Map k a -> a
mapLookupM k m = maybeToMonoid $ Map.lookup k m

mapUpdateM :: (Monoid a, Ord k) => (a -> a) -> k -> Map.Map k a -> Map.Map k a
mapUpdateM f = Map.alter (Just . f . maybeToMonoid)

type Mos k a = Map.Map k (Set.Set a)

mosInsert :: (Ord k, Ord a) => k -> a -> Mos k a -> Mos k a
mosInsert k a = mapUpdateM (Set.insert a) k

mosDelete :: (Ord k, Ord a) => k -> a -> Mos k a -> Mos k a
mosDelete k a = Map.update f k
  where
    f = foldableToMaybe . (Set.delete a)

invertMap :: (Ord k, Ord a) => Map.Map k a -> Mos a k
invertMap = Map.foldrWithKey (flip mosInsert) mempty

mosDifference :: (Ord k, Ord a) => Mos k a -> Mos k a -> Mos k a
mosDifference = merge preserveMissing dropMissing (zipWithMaybeMatched f)
  where
    f k sa1 sa2 = foldableToMaybe $ Set.difference sa1 sa2

mosUnion :: (Ord k, Ord a) => Mos k a -> Mos k a -> Mos k a
mosUnion = merge preserveMissing preserveMissing (zipWithMatched f)
  where
    f k sa1 sa2 = Set.union sa1 sa2

type Mol k a = Map.Map k [a]

molFromList :: (Ord k) => [(k, a)] -> Mol k a
molFromList = foldr (uncurry molCons) mempty

molToList :: (Ord k) => Mol k a -> [(k, a)]
molToList mol = mconcat $ sequence <$> Map.toList mol

molCons :: (Ord k) => k -> a -> Mol k a -> Mol k a
molCons k a = mapUpdateM (a :) k

molAppend :: (Ord k) => k -> a -> Mol k a -> Mol k a
molAppend k a = mapUpdateM (++ [a]) k


data Interpolation = IConstant | ILinear | IBezier Word32 Word32
  deriving (Eq, Show)

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
    _getTypeUseageMap :: Mos TypePath NodePath
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
        (pathToString $ maybeToMonoid $ Map.lookup path typeMap)
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
    allPaths = Set.fromList $ fmap fst nodes
    orphans = Set.difference allPaths allChildPaths
    missing = Set.difference allChildPaths allPaths

treeInitNode :: NodePath -> TypePath -> ClapiTree a -> ClapiTree a
treeInitNode path typePath (ClapiTree nodeMap typeMap typeUsedByMap) =
    ClapiTree newNodeMap newTypeMap newTypeUsedByMap
  where
    newNodeMap = Map.insert path mempty nodeMap
    newTypeMap = Map.insert path typePath typeMap
    maybeOldTypePath = Map.lookup path typeMap
    newTypeUsedByMap =
        mosDelete' maybeOldTypePath path $
        mosInsert typePath path $
        typeUsedByMap
    mosDelete' Nothing _ = id
    mosDelete' (Just k) a = mosDelete k a



treeDelete :: NodePath -> ClapiTree a -> CanFail (ClapiTree a)
treeDelete path (ClapiTree nodeMap typeMap typesUsedByMap) =
    Right $ ClapiTree remainingNodes newTypeMap newUsedByMap
  where
    (removedNodes, remainingNodes) = Map.partitionWithKey f nodeMap
    f perhapsChildPath _ = isPrefixOf path perhapsChildPath
    removedTypePaths = Map.mapMaybeWithKey lookupOldType removedNodes
    lookupOldType nodePath _ = Map.lookup nodePath typeMap
    newTypeMap = Map.difference typeMap removedTypePaths
    newUsedByMap = mosDifference typesUsedByMap $ invertMap removedTypePaths

treeSetChildren :: NodePath -> [Name] -> ClapiTree a -> CanFail (ClapiTree a)
treeSetChildren path keys tree = at path f tree
  where
    f Nothing = Left $ printf "not found %s" (show path)
    f (Just node) = Right . Just $ over getKeys (const keys) node

type TreeAction a = Maybe (Attributed (Maybe (TimePoint a))) ->
    CanFail (Maybe (Attributed (Maybe (TimePoint a))))
treeAction :: TreeAction a -> NodePath -> Maybe Site -> Time -> ClapiTree a ->
    CanFail (ClapiTree a)
treeAction action path maybeSite t tree =
  do
    node <- (note $ printf "not found %s" (show path)) $ view (at path) tree
    existingTimeSeries <- return $ view (getSites . (ix maybeSite)) node
    newTimeSeries <- at t action existingTimeSeries
    newNode <- return $
        node & (getSites . (at maybeSite)) .~ (foldableToMaybe newTimeSeries)
    newTree <- return $ tree & (at path) .~ (Just newNode)
    return newTree


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


data Delta a = Remove | Add a | Change a deriving (Eq, Show)

instance Functor Delta where
    fmap f Remove = Remove
    fmap f (Add a) = Add (f a)
    fmap f (Change a) = Change (f a)


mapDiff :: (Ord k, Eq a) => Map.Map k a -> Map.Map k a -> Map.Map k (Delta a)
mapDiff m1 m2 = merge onlyInM1 onlyInM2 inBoth m1 m2
  where
    onlyInM1 = mapMissing $ \k v1 -> Remove
    onlyInM2 = mapMissing $ \k v2 -> Add v2
    inBoth = zipWithMaybeMatched diffValue
    diffValue _ x y
        | x == y = Nothing
        | otherwise = Just (Change y)

applyMapDiff :: Ord k => Map.Map k (Delta a) -> Map.Map k a -> Map.Map k a
applyMapDiff d m = merge onlyInD onlyInM inBoth d m
  where
    onlyInD = mapMissing onlyInDf
    onlyInDf _ (Add v) = v
    -- FIXME: These undefineds are bad error handling...
    onlyInDf _ _ = undefined
    onlyInM = mapMissing $ \k v -> v
    inBoth = zipWithMaybeMatched inBothf
    inBothf _ (Change v) _ = Just v
    inBothf _ (Remove) _ = Nothing


diffMaybe :: Maybe a -> Maybe a -> Maybe (Delta a)
diffMaybe (Just x) (Just y) = Just $ Change y
diffMaybe Nothing (Just y) = Just $ Add y
diffMaybe (Just x) Nothing = Just Remove
diffMaybe _ _ = Nothing

applyDiffMaybe :: Maybe (Delta a) -> Maybe a -> Maybe a
applyDiffMaybe Nothing _ = Nothing
applyDiffMaybe (Just Remove) (Just _) = Nothing
applyDiffMaybe (Just (Add y)) Nothing = Just y
applyDiffMaybe (Just (Change y)) (Just x) = Just y
applyDiffMaybe _ _ = undefined


diffEither :: Either a b -> Either a b -> Either (Delta a) (Delta b)
diffEither (Left xa) (Left ya) = Left (Change ya)
diffEither (Left xa) (Right yb) = Right (Add yb)
diffEither (Right xb) (Left ya) = Left (Add ya)
diffEither (Right xb) (Right yb) = Right (Change yb)

applyDiffEither :: Either (Delta a) (Delta b) -> Either a b -> Either a b
applyDiffEither (Left (Change a)) (Left _) = Left a
applyDiffEither (Right (Change b)) (Right _) = Right b
applyDiffEither (Left (Add a)) (Right _) = Left a
applyDiffEither (Right (Add b)) (Left _) = Right b
