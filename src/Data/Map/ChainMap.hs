module Data.Map.ChainMap where

import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

type ChainMap k a = NonEmpty.NonEmpty (Map.Map k a)

fromMap :: Ord k => Map.Map k a -> ChainMap k a
fromMap = pure

toMap :: Ord k => ChainMap k a -> Map.Map k a
toMap = Map.unions . NonEmpty.toList

fromList :: Ord k => [(k, a)] -> ChainMap k a
fromList = fromMap . Map.fromList

toList :: Ord k => ChainMap k a -> [(k, a)]
toList = Map.toList . toMap

extend :: Ord k => Map.Map k a -> ChainMap k a -> ChainMap k a
extend m1 (m2 :| ms) = m1 :| m2 : ms

grow :: Ord k => ChainMap k a -> ChainMap k a
grow = extend mempty

-- FIXME: Possible to fail?
shrink :: Ord k => ChainMap k a -> ChainMap k a
shrink (m1 :| m2 : ms) = Map.union m1 m2 :| ms
shrink ms = ms

insert :: Ord k => k -> a -> ChainMap k a -> ChainMap k a
insert k a (m :| ms) = Map.insert k a m :| ms

delete :: Ord k => k -> ChainMap k a -> ChainMap k a
delete k = fmap (Map.delete k)
-- FIXME: or
-- delete k (m :| ms) = Map.delete k m :| ms

lookup :: Ord k => k -> ChainMap k a -> Maybe a
lookup k = foldl (\a m -> a <|> Map.lookup k m) Nothing
