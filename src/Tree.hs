module Tree
    (
        Interpolation(..),
        TimePoint,
        TimeSeries,
        Tuple(..),
        ClapiTree(..),
        treeGet, treeAdd, treeSet, treeDelete,
        mapDiff, Delta(..), Diff(..)
    )
where

import Data.Word (Word32, Word64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict.Merge (merge, mapMissing, zipWithMatched)
import Control.Applicative (Const(..))

import Types (Name, ClapiPath(..), Time, ClapiValue)

data Interpolation = IConstant | ILinear | IBezier Word32 Word32
  deriving (Eq, Show)

type TimePoint a = (a, Interpolation)
type TimeSeries a = Map.Map Time (TimePoint a)

data Tuple = TConstant [ClapiValue] | TDynamic (TimeSeries [ClapiValue])
  deriving (Eq, Show)

data ClapiTree =
    Leaf {typePath :: ClapiPath, tuple :: Tuple} |
    Container {
        typePath :: ClapiPath,
        order :: [Name],
        subtrees :: Map.Map Name ClapiTree
    }
  deriving (Eq, Show)

treeGet :: ClapiPath -> ClapiTree -> Either String ClapiTree
treeGet path = getConst . alterTree (Const . Left) get path
  where
    get :: Maybe ClapiTree -> Const (Either String ClapiTree) (Maybe ClapiTree)
    get Nothing = Const (Left $ "Item lookup failed" ++ (show path))
    get (Just tree) = Const (Right tree)

type AlterF f a = Maybe a -> f (Maybe a)
type MakeError f a = String -> f a

treeDelete :: ClapiPath -> ClapiTree -> Either String ClapiTree
treeDelete path = alterTree Left delete path
  where
    delete :: AlterF (Either String) ClapiTree
    delete Nothing = Left $ "Tried to delete absent value at " ++ (show path)
    delete _ = Right Nothing

treeAdd :: ClapiTree -> ClapiPath -> ClapiTree -> Either String ClapiTree
treeAdd newItem path = alterTree Left add path
  where
    add :: AlterF (Either String) ClapiTree
    add Nothing = Right . Just $ newItem
    add _ = Left $ "Tried to add over present value at " ++ (show path)

treeSet :: ClapiTree -> ClapiPath -> ClapiTree -> Either String ClapiTree
treeSet replacementItem path = alterTree Left set path
  where
    set :: AlterF (Either String) ClapiTree
    set (Just tree) = Right . Just $ replacementItem
    set _ = Left $ "Tried to set at absent value at " ++ (show path)

alterTree ::
    Functor f =>
    MakeError f ClapiTree -> AlterF f ClapiTree ->
    ClapiPath -> ClapiTree -> f ClapiTree
alterTree makeError f rootPath tree = alterTree' rootPath (Just tree)
  where
    -- alterTree' :: ClapiPath -> Maybe ClapiTree -> f ClapiTree
    alterTree' _ Nothing = makeError "Intermediate lookup failed"
    alterTree' (name:path) (Just c@(Container {subtrees = items})) =
        fmap (\is -> c {subtrees = is}) (Map.alterF chooseF name items)
      where
        -- chooseF :: ClapiPath -> AlterF f ClapiTree
        chooseF = case path of
            [] -> f
            path -> \maybeTree -> fmap (Just) (alterTree' path maybeTree)
    alterTree' _ _ = makeError "Lookup failed (not a container)"


data Delta k a = Remove k | Add k a | Change k a a deriving (Eq, Show)
type Diff k a = [Delta k a]

mapDiff :: Ord k => Map.Map k a -> Map.Map k a -> Diff k a
mapDiff a b = flatten $ merge onlyInA onlyInB inBoth a b
  where
    onlyInA = mapMissing $ \k va -> Remove k
    onlyInB = mapMissing $ \k vb -> Add k vb
    inBoth = zipWithMatched $ \k va vb -> Change k va vb
    flatten = (fmap snd) . Map.toList
