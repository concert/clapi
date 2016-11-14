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
    Container {typePath :: ClapiPath, subtrees :: Map.Map Name ClapiTree}
  deriving (Eq, Show)

treeGet :: ClapiPath -> ClapiTree -> Maybe ClapiTree
treeGet path tree = get path (Just tree)
  where
    get _ Nothing = Nothing
    get [] x = x
    get (name:path) (Just (Container _ items)) =
        get path $ Map.lookup name items
    get _ _ = Nothing

type AlterF f a = Maybe a -> f (Maybe a)

treeDelete :: ClapiPath -> ClapiTree -> Maybe ClapiTree
treeDelete = alterTree delete
  where
    delete :: AlterF Maybe ClapiTree
    delete Nothing = Nothing
    delete _ = Just Nothing

treeAdd :: ClapiTree -> ClapiPath -> ClapiTree -> Maybe ClapiTree
treeAdd newItem = alterTree add
  where
    add :: AlterF Maybe ClapiTree
    add Nothing = Just . Just $ newItem
    add _ = Nothing

treeSet :: ClapiTree -> ClapiPath -> ClapiTree -> Maybe ClapiTree
treeSet replacementItem = alterTree set
  where
    set :: AlterF Maybe ClapiTree
    set (Just tree) = Just . Just $ replacementItem
    set _ = Nothing

alterTree ::
    AlterF Maybe ClapiTree -> ClapiPath -> ClapiTree -> Maybe ClapiTree
alterTree f path tree = alterTree' f path (Just tree)

alterTree' ::
    AlterF Maybe ClapiTree -> ClapiPath -> Maybe ClapiTree -> Maybe ClapiTree
alterTree' _ _ Nothing = Nothing
alterTree' f (name:path) (Just (Container typePath items)) =
    fmap (Container typePath) (Map.alterF alt name items)
  where
    alt = case path of
        [] -> f
        path -> internalF
    internalF :: AlterF Maybe ClapiTree
    internalF Nothing = Nothing
    internalF tree = fmap (Just) (alterTree' f path tree)
alterTree' _ _ _ = Nothing
