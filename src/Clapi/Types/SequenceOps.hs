{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    DeriveFunctor
#-}

module Clapi.Types.SequenceOps
  ( SequenceOp(..), isSoAbsent, isSoCreate
  , updateUniqList
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Foldable (foldlM)

import Clapi.Types.UniqList
  (UniqList, unUniqList, mkUniqList, ulDelete, ulInsert)
import Clapi.Util (ensureUnique)

data SequenceOp i d
  = SoCreateAfter (Maybe i) d
  | SoMoveAfter (Maybe i)
  | SoAbsent
  deriving (Show, Eq, Functor)

isSoAbsent :: SequenceOp i d -> Bool
isSoAbsent so = case so of
  SoAbsent -> True
  _ -> False

isSoCreate :: SequenceOp i d -> Bool
isSoCreate so = case so of
  SoCreateAfter _ _ -> True
  _ -> False

updateUniqList
  :: (Eq i, Ord i, Show i, MonadFail m)
  => Map i (SequenceOp i d) -> UniqList i -> m (UniqList i)
updateUniqList ops ul = do
    ensureUnique "flange" $ Map.elems reorders
    reorderFromDeps reorders $ Map.foldlWithKey foo ul ops
  where
    foo ul' i op = case op of
      SoCreateAfter _ _ -> ulInsert i ul'
      SoMoveAfter _ -> ul'
      SoAbsent -> ulDelete i ul'
    reorders = Map.foldlWithKey bar mempty ops
    bar acc i op = case op of
      SoCreateAfter mi _ -> Map.insert i mi acc
      SoMoveAfter mi -> Map.insert i mi acc
      SoAbsent -> acc

getChainStarts ::
    Ord i => Map i (Maybe i) -> ([(i, Maybe i)], Map i (Maybe i))
getChainStarts m =
    let
        hasUnresolvedDep = maybe False (flip Map.member m)
        (remainder, starts) = Map.partition hasUnresolvedDep m
    in (Map.toList starts, remainder)

reorderFromDeps
    :: (MonadFail m, Ord i, Show i)
    => Map i (Maybe i) -> UniqList i -> m (UniqList i)
reorderFromDeps m ul =
    resolveDigest m >>= applyMoves (unUniqList ul) >>= mkUniqList
  where
    resolveDigest m' = if null m' then return []
        else case getChainStarts m' of
            ([], _) -> fail "Unresolvable order dependencies"
            (starts, remainder) -> (starts ++) <$> resolveDigest remainder
    applyMoves l starts = foldlM applyMove l starts

applyMove :: (MonadFail m, Eq i, Show i) => [i] -> (i, Maybe i) -> m [i]
applyMove l (i, mi) =
    removeElem "Element was not present to move" i l
    >>= insertAfter "Preceeding element not found for move" i mi


insertAfter
  :: (MonadFail m, Eq i, Show i) => String -> i -> Maybe i -> [i] -> m [i]
insertAfter msg v mAfter ol = case mAfter of
    Nothing -> return $ v : ol
    Just after ->
      let
        (bl, al) = span (/= after) ol
      in case al of
        (a:rl) -> return $ bl ++ [a, v] ++ rl
        [] -> fail $ msg ++ ": " ++ show after

removeElem
  :: (MonadFail m, Eq i, Show i) => String -> i -> [i] -> m [i]
removeElem msg v ol =
  let
    (ds, ol') = List.partition (== v) ol
  in case ds of
    [_] -> return ol'
    _ -> fail $ msg ++ ": " ++ show v
