{-# OPTIONS_GHC -Wall -Wno-orphans #-}
module Clapi.Types.SequenceOps
  ( ReorderBundle(..), SequenceOp(..)
  , digest, applyDigest
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Foldable (foldlM)

import Clapi.Types.UniqList (UniqList, unUniqList, mkUniqList)

data SequenceOp i
  = AddAfter (Maybe i)
  | MoveAfter (Maybe i)
  | DelElem

newtype ReorderBundle i = ReorderBundle [(i, SequenceOp i)]
newtype SequenceDigest i = SequenceDigest (Map.Map i (SequenceOp i))

sdEmpty :: SequenceDigest i -> Bool
sdEmpty (SequenceDigest m) = null m

digest :: Ord i => ReorderBundle i -> SequenceDigest i
digest (ReorderBundle ops) = SequenceDigest $ Map.fromList ops

getChainStarts ::
    Ord i => SequenceDigest i -> ([(i, SequenceOp i)], SequenceDigest i)
getChainStarts (SequenceDigest m) =
    let
        getDep (AddAfter mi) = mi
        getDep (MoveAfter mi) = mi
        getDep DelElem = Nothing
        hasUnresolvedDep = maybe False (flip Map.member m) . getDep
        (remainder, starts) = Map.partition hasUnresolvedDep m
    in (Map.toList starts, SequenceDigest remainder)

applyDigest
    :: (MonadFail m, Ord i, Show i)
    => SequenceDigest i -> UniqList i -> m (UniqList i)
applyDigest sd ul =
    resolveDigest sd >>= applyOps (unUniqList ul) >>= mkUniqList
  where
    resolveDigest sd' = if sdEmpty sd' then return []
        else case getChainStarts sd' of
            ([], _) -> fail "Unresolvable order dependencies"
            (starts, remainder) -> (starts ++) <$> resolveDigest remainder
    applyOps l starts = foldlM applyOp l starts

applyOp :: (MonadFail m, Ord i, Show i) => [i] -> (i, SequenceOp i) -> m [i]
applyOp l (i, op) = case op of
    AddAfter mi -> if i `elem` l
        then fail "Cannot add element twice"
        else insertAfter i mi l
    MoveAfter mi ->
        removeElem "Element was not present to move" i l >>= insertAfter i mi
    DelElem -> removeElem "Element not present to remove" i l
  where
    insertAfter v mAfter ol = case mAfter of
        Nothing -> return $ v : ol
        Just after ->
          let
            (bl, al) = span (/= after) ol
          in case al of
            (a:rl) -> return $ bl ++ [a, v] ++ rl
            [] -> fail "Cannot add after non-existant element"
    removeElem msg v ol =
      let
        (ds, ol') = List.partition (== v) ol
      in case ds of
        [_] -> return ol'
        _ -> fail $ msg ++ ": " ++ show v
