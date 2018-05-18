{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveLift #-}

module Clapi.Types.Path (
    Path(..), Seg, mkSeg, unSeg, joinSegs,
    pathP, segP, toText, fromText,
    splitHead, splitTail, parentPath,
    pattern Root, pattern (:</), pattern (:/),
    isParentOf, isChildOf, isParentOfAny, isChildOfAny, childPaths,
    NodePath,
    TypeName(..), typeNameP, typeNameToText, typeNameFromText) where

import Prelude hiding (fail)
import qualified Data.Attoparsec.Text as DAT
import Data.Attoparsec.Text (Parser)
import Data.Char (isLetter, isDigit)
import Data.List (isPrefixOf)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Fail (MonadFail, fail)
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (Lift)

newtype Seg = Seg {unSeg :: Text} deriving (Eq, Ord, Lift)

instance Show Seg where
    show = show . unSeg

isValidSegChar :: Char -> Bool
isValidSegChar c = isLetter c || isDigit c || c == '_'

segP :: Parser Seg
segP = fmap (Seg . Text.pack) $ DAT.many1 $ DAT.satisfy isValidSegChar

mkSeg :: MonadFail m => Text -> m Seg
mkSeg = either fail return . DAT.parseOnly (segP <* DAT.endOfInput)

joinSegs :: [Seg] -> Seg
joinSegs = Seg . Text.intercalate (Text.singleton '_') . fmap unSeg

newtype Path = Path {unPath :: [Seg]} deriving (Eq, Ord, Lift)

sepChar :: Char
sepChar = '/'

sepText :: Text
sepText = Text.singleton sepChar

instance Show Path where
    show = Text.unpack . toText

toText :: Path -> Text
toText (Path segs) = sepText <> Text.intercalate sepText (fmap unSeg segs)

pattern Root :: Path
pattern Root = Path []

splitHead :: Path -> Maybe (Seg, Path)
splitHead (Path []) = Nothing
splitHead (Path (seg:segs)) = Just (seg, Path segs)

pattern (:</) :: Seg -> Path -> Path
pattern seg :</ path <- (splitHead -> Just (seg, path)) where
    seg :</ path = Path $ seg : unPath path

splitTail :: Path -> Maybe (Path, Seg)
splitTail (Path path) = case path of
    (y : xs) -> (\(s, ps) -> Just (Path ps, s)) $ go y xs
    [] -> Nothing
  where
    go :: Seg -> [Seg] -> (Seg, [Seg])
    go y xs = case xs of
        [] -> (y, [])
        (z : zs) -> (y :) <$> go z zs

pattern (:/) :: Path -> Seg -> Path
pattern path :/ seg <- (splitTail -> Just (path, seg)) where
    path :/ seg = Path $ unPath path ++ [seg]

pathP :: Parser Path
pathP = let sepP = DAT.char sepChar in
    fmap Path $ sepP >> segP `DAT.sepBy` sepP

fromText :: MonadFail m => Text -> m Path
fromText = either fail return . DAT.parseOnly (pathP <* DAT.endOfInput)

isParentOf :: Path -> Path -> Bool
isParentOf (Path a) (Path b) = isPrefixOf a b

isChildOf :: Path -> Path -> Bool
isChildOf = flip isParentOf

isParentOfAny :: (Functor f, Foldable f) => Path -> f Path -> Bool
isParentOfAny parent candidates = or $ isParentOf parent <$> candidates

isChildOfAny :: (Functor f, Foldable f) => Path -> f Path -> Bool
isChildOfAny candidateChild parents = or $ isChildOf candidateChild <$> parents

childPaths :: Functor f => Path -> f Seg -> f Path
childPaths (Path segs) ss = Path . (segs ++) . pure <$> ss

type NodePath = Path

data TypeName = TypeName {tnNamespace :: Seg, tnName :: Seg} deriving (Eq, Ord)

qualSepChar :: Char
qualSepChar = ':'

typeNameToText :: TypeName -> Text
typeNameToText (TypeName ns s) =
  unSeg ns <> Text.singleton qualSepChar <> unSeg s

instance Show TypeName where
  show = Text.unpack . typeNameToText

typeNameP :: Parser TypeName
typeNameP = TypeName <$> segP <*> (DAT.char qualSepChar >> segP)

typeNameFromText :: MonadFail m => Text -> m TypeName
typeNameFromText =
  either fail return . DAT.parseOnly (typeNameP <* DAT.endOfInput)

parentPath :: Path -> Maybe Path
parentPath p = case p of
  (pp :/ _) -> Just pp
  _ -> Nothing
