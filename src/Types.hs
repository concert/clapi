module Types
    (
        Time(..),
        ClapiValue(..),
        fromClapiValue,
        toClapiValue,
        ClapiPath,
        root,
        up,
        ClapiMethod(..),
        ClapiMessage(..),
        ClapiBundle,
        ClapiMessageTag,
        Interpolation(..),
        TimePoint,
        TimeSeries,
        Tuple(..),
        ClapiTree(..),
        treeGet, treeAdd, treeSet, treeDelete,
    )
where

import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Applicative (Const(..))

data ClapiMessage = CMessage {
    msgPath :: ClapiPath,
    msgMethod :: ClapiMethod,
    msgArgs :: [ClapiValue],
    msgTags :: [ClapiMessageTag]
} deriving (Eq, Show)

data Time = Time Word64 Word32 deriving (Eq, Show, Ord, Bounded)

data ClapiValue = CBool Bool | CTime Time |
    CWord32 Word32 | CWord64 Word64 |
    CInt32 Int32 | CInt64 Int64 |
    CFloat Float | CDouble Double |
    CString T.Text | CList [ClapiValue] deriving (Eq, Show)

class Clapiable a where
    toClapiValue :: a -> ClapiValue
    fromClapiValue :: ClapiValue -> a

instance Clapiable Bool where
    toClapiValue = CBool
    fromClapiValue (CBool x) = x

instance Clapiable Time where
    toClapiValue = CTime
    fromClapiValue (CTime x) = x

instance Clapiable Word32 where
    toClapiValue = CWord32
    fromClapiValue (CWord32 x) = x

instance Clapiable Word64 where
    toClapiValue = CWord64
    fromClapiValue (CWord64 x) = x

instance Clapiable Int32 where
    toClapiValue = CInt32
    fromClapiValue (CInt32 x) = x

instance Clapiable Int64 where
    toClapiValue = CInt64
    fromClapiValue (CInt64 x) = x

instance Clapiable Float where
    toClapiValue = CFloat
    fromClapiValue (CFloat x) = x

instance Clapiable Double where
    toClapiValue = CDouble
    fromClapiValue (CDouble x) = x

instance Clapiable T.Text where
    toClapiValue = CString
    fromClapiValue (CString x) = x

instance Clapiable a => Clapiable [a] where
    toClapiValue = CList . (map toClapiValue)
    fromClapiValue (CList xs) = map fromClapiValue xs


type Name = String
type ClapiPath = [Name]

root :: ClapiPath
root = []

up :: ClapiPath -> ClapiPath
up [] = root
-- FIXME: using Data.Seq would be faster than a built in list for init (removing
-- last element)
up cs = init cs

data ClapiMethod = Error | Set | Add | Remove | Clear | Subscribe |
    Unsubscribe | AssignType | Children | Delete |
    Identify deriving (Eq, Show, Read, Enum, Bounded)

type ClapiMessageTag = (String, ClapiValue)

type ClapiBundle = [ClapiMessage]

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
