{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveLift #-}

module Clapi.Types.Path
  ( Seg, mkSeg, unSeg, segP, Placeholder(..), Namespace(..)
  , Path'(..), Path, pathP, toText, fromText
  , pattern Root, pattern (:</), pattern (:/)
  , splitHead, splitTail, parentPath
  , isParentOf, isChildOf, isParentOfAny, isChildOfAny, childPaths
  , prefixes, prefixesMap
  ) where

import Prelude hiding (fail)
import qualified Data.Attoparsec.Text as DAT
import Data.Attoparsec.Text (Parser)
import Data.Char (isLetter, isDigit)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Fail (MonadFail, fail)
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (Lift)

newtype Seg = Seg {unSeg :: Text} deriving (Eq, Ord, Lift)

instance Show Seg where
    show = Text.unpack . unSeg

isValidSegChar :: Char -> Bool
isValidSegChar c = isLetter c || isDigit c || c == '_'

segP :: Parser Seg
segP = fmap (Seg . Text.pack) $ DAT.many1 $ DAT.satisfy isValidSegChar

mkSeg :: MonadFail m => Text -> m Seg
mkSeg = either fail return . DAT.parseOnly (segP <* DAT.endOfInput)

instance Semigroup Seg where
  (Seg t1) <> (Seg t2) = Seg (t1 <> Text.singleton '_' <> t2)

newtype Namespace = Namespace {unNamespace :: Seg} deriving (Show, Eq, Ord)
newtype Placeholder
  = Placeholder { unPlaceholder :: Seg } deriving (Eq, Ord, Show)

newtype Path' a = Path' {unPath :: [a]} deriving (Eq, Ord, Lift)
type Path = Path' Seg

sepChar :: Char
sepChar = '/'

sepText :: Text
sepText = Text.singleton sepChar

instance Show a => Show (Path' a) where
    show = Text.unpack . toText (Text.pack . show)

toText :: (a -> Text) -> Path' a -> Text
toText f (Path' as) = sepText <> Text.intercalate sepText (fmap f as)

pattern Root :: Path' a
pattern Root = Path' []

splitHead :: Path' a -> Maybe (a, Path' a)
splitHead (Path' []) = Nothing
splitHead (Path' (seg:segs)) = Just (seg, Path' segs)

pattern (:</) :: a -> Path' a -> Path' a
pattern a :</ path <- (splitHead -> Just (a, path)) where
    a :</ path = Path' $ a : unPath path

splitTail :: Path' a -> Maybe (Path' a, a)
splitTail (Path' path) = case path of
    (y : xs) -> (\(s, ps) -> Just (Path' ps, s)) $ go y xs
    [] -> Nothing
  where
    go :: a -> [a] -> (a, [a])
    go y xs = case xs of
        [] -> (y, [])
        (z : zs) -> (y :) <$> go z zs

pattern (:/) :: Path' a -> a -> Path' a
pattern path :/ a <- (splitTail -> Just (path, a)) where
    path :/ a = Path' $ unPath path ++ [a]

pathP :: Parser a -> Parser (Path' a)
pathP p = let sepP = DAT.char sepChar in
    fmap Path' $ sepP >> p `DAT.sepBy` sepP

fromText :: MonadFail m => Parser a -> Text -> m (Path' a)
fromText p = either fail return . DAT.parseOnly (pathP p <* DAT.endOfInput)

isParentOf :: Eq a => Path' a -> Path' a -> Bool
isParentOf (Path' as1) (Path' as2) = isPrefixOf as1 as2

isChildOf :: Eq a => Path' a -> Path' a -> Bool
isChildOf = flip isParentOf

isParentOfAny :: (Eq a, Functor f, Foldable f) => Path' a -> f (Path' a) -> Bool
isParentOfAny parent candidates = or $ isParentOf parent <$> candidates

isChildOfAny :: (Eq a, Functor f, Foldable f) => Path' a -> f (Path' a) -> Bool
isChildOfAny candidateChild parents = or $ isChildOf candidateChild <$> parents

childPaths :: Functor f => Path' a -> f a -> f (Path' a)
childPaths (Path' as1) as2 = Path' . (as1 ++) . pure <$> as2

parentPath :: Path' a -> Maybe (Path' a)
parentPath p = case p of
  (pp :/ _) -> Just pp
  _ -> Nothing

prefixes :: Ord a => Set (Path' a) -> Set (Path' a)
prefixes = Map.keysSet . prefixesMap . Map.fromSet (const ())

-- FIXME: Might want to test the property that
-- `Map.valid (prefixesMap ps) == True` for all ps
prefixesMap :: Eq k => Map (Path' k) v -> Map (Path' k) v
prefixesMap = Map.fromAscList . f . Map.toAscList
  where
    f [] = []
    f ((p,v):pvs) = (p, v) : g p v pvs
    g _ _ [] = []
    g p1 v1 ((p2,v2):pvs) = if p1 `isParentOf` p2
      then g p1 v1 pvs
      else (p2, v2) : g p2 v2 pvs
