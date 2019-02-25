{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveLift #-}

module Clapi.Types.Path
  ( Name, mkName, unName, nameP, Placeholder(..), Namespace(..)
  , Path'(..), Path, pathP, toText, fromText
  , pattern Root, pattern (:</), pattern (:/)
  , splitHead, splitTail, parentPath
  , isParentOf, isStrictParentOf, isChildOf, isStrictChildOf, childPaths
  , isParentOfAny, isStrictParentOfAny, isChildOfAny, isStrictChildOfAny
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

newtype Name = Name {unName :: Text} deriving (Eq, Ord, Lift)

instance Show Name where
    show = Text.unpack . unName

isValidSegChar :: Char -> Bool
isValidSegChar c = isLetter c || isDigit c || c == '_'

nameP :: Parser Name
nameP = fmap (Name . Text.pack) $ DAT.many1 $ DAT.satisfy isValidNameChar

mkName :: MonadFail m => Text -> m Name
mkName = either fail return . DAT.parseOnly (nameP <* DAT.endOfInput)

instance Semigroup Name where
  (Name t1) <> (Name t2) = Name (t1 <> Text.singleton '_' <> t2)

newtype Namespace = Namespace {unNamespace :: Name} deriving (Show, Eq, Ord)
newtype Placeholder
  = Placeholder { unPlaceholder :: Name } deriving (Eq, Ord, Show)

newtype Path' a = Path' {unPath :: [a]} deriving (Eq, Ord, Lift)
type Path = Path' Name

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
splitHead (Path' (name:names)) = Just (name, Path' names)

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

isStrictParentOf :: Eq a => Path' a -> Path' a -> Bool
isStrictParentOf p1 p2 = p1 `isParentOf` p2 && p1 /= p2

isChildOf :: Eq a => Path' a -> Path' a -> Bool
isChildOf = flip isParentOf

isStrictChildOf :: Eq a => Path' a -> Path' a -> Bool
isStrictChildOf = flip isStrictParentOf

ofAny
  :: Foldable f
  => (Path' a -> Path' a -> Bool) -> Path' a -> f (Path' a) -> Bool
ofAny f candidate = any (f candidate)

isParentOfAny, isStrictParentOfAny, isChildOfAny, isStrictChildOfAny
  :: (Eq a, Foldable f) => Path' a -> f (Path' a) -> Bool
isParentOfAny = ofAny isParentOf
isStrictParentOfAny = ofAny isStrictParentOf
isChildOfAny = ofAny isParentOf
isStrictChildOfAny = ofAny isStrictChildOf

childPaths :: Functor f => Path' a -> f a -> f (Path' a)
childPaths (Path' as1) as2 = Path' . (as1 ++) . pure <$> as2

parentPath :: Path' a -> Maybe (Path' a)
parentPath p = case p of
  (pp :/ _) -> Just pp
  _ -> Nothing

prefixes :: Eq a => Set (Path' a) -> Set (Path' a)
prefixes = Map.keysSet . prefixesMap . Map.fromSet (const ())

prefixesMap :: Eq k => Map (Path' k) v -> Map (Path' k) v
prefixesMap = Map.fromAscList . f . Map.toAscList
  where
    f [] = []
    f ((p, v):pvs) = (p, v) : g p v pvs

    g _ _ [] = []
    g p1 v1 ((p2, v2):pvs) = if p1 `isParentOf` p2
      then g p1 v1 pvs
      else (p2, v2) : g p2 v2 pvs
