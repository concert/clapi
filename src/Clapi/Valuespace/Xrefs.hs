{-# LANGUAGE
    LambdaCase
#-}

module Clapi.Valuespace.Xrefs
  ( Referer(..), Referee(..)
  -- FIXME: Might want TypeAssertionCache to be internal so that just the tests
  -- can get at the constructors
  , TypeAssertions, toTypeAssertions, TypeAssertionCache(..), empty
  , updateConst, updateTp, removeConst, removeTp
  , lookup, referers
  ) where


import Prelude hiding (lookup)
import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Clapi.Types.Definitions (DefName)
import Clapi.Types.Base (TpId)
import Clapi.Types.Path (Path)
import Clapi.Validator (TypeAssertion(..))


newtype Referer = Referer {unReferer :: Path} deriving (Show, Eq, Ord)
newtype Referee = Referee {unReferee :: Path} deriving (Show, Eq, Ord)

type TypeAssertions = Map Referee DefName

toTypeAssertions :: Foldable f => f TypeAssertion -> TypeAssertions
toTypeAssertions = Map.fromList
  . fmap (\(TypeAssertion p dn) -> (Referee p, dn))
  . toList


data TypeAssertionCache
  = TypeAssertionCache
  { tacRefererTypeAssertions
      :: Map Referer (Either TypeAssertions (Map TpId TypeAssertions))
  , tacRefereeReferers :: Mos Referee (Referer, Maybe TpId)
  , tacRefereeDns :: Map Referee DefName
  } deriving Show


empty :: TypeAssertionCache
empty = TypeAssertionCache mempty mempty mempty

updateDataPoint
  :: Referer -> Maybe TpId -> TypeAssertions -> TypeAssertionCache
  -> TypeAssertionCache
updateDataPoint referer mtpid newTas tac =
  let
    (tasToRemove, rtas') = Map.alterF thingAtTra referer
      $ tacRefererTypeAssertions tac
    trrs' = Map.foldlWithKey
      -- Insert the new:
      (\acc referee _dn -> Mos.insert referee (referer, mtpid) acc)
      -- Remove the old:
      (maybe id
        (either
          (deleteOldTas Nothing)
          (\tasMap acc -> Map.foldlWithKey
            (\acc' tpid tas -> deleteOldTas (Just tpid) tas acc') acc tasMap)
        ) tasToRemove $ tacRefereeReferers tac
      ) newTas
    trds' = newTas <> tacRefereeDns tac
  in
    TypeAssertionCache rtas' trrs' trds'
  where
    thingAtTra
      :: Maybe (Either TypeAssertions (Map TpId TypeAssertions))
      -> ( Maybe (Either TypeAssertions (Map TpId TypeAssertions))
         , Maybe (Either TypeAssertions (Map TpId TypeAssertions)))
    thingAtTra x =
      let
        mNewTas = if null newTas then Nothing else Just newTas
        wrapTas = case mtpid of
          Nothing -> Left
          Just tpid -> Right . Map.singleton tpid
        y@(_toRemove, _newValue) = case x of
          -- Exiting time point assertions:
          Just (Right tasMap) -> case mtpid of
            -- Overwrite entire time series:
            Nothing -> (x, wrapTas <$> mNewTas)
            -- Edit with new time point's type assertions:
            Just tpid ->
              bimap (fmap wrapTas) (Just . Right) $
              Map.alterF (\tpidTas -> (tpidTas, mNewTas)) tpid tasMap
          -- Previously no type assertions or type assertions about a constant
          -- node, overwrite entire time series:
          _ -> (x, wrapTas <$> mNewTas)
      in y

    deleteOldTas
      :: Maybe TpId -> TypeAssertions -> Mos Referee (Referer, Maybe TpId)
      -> Mos Referee (Referer, Maybe TpId)
    deleteOldTas mtpid' = flip $ Map.foldlWithKey
        (\acc referee _dn -> Mos.delete referee (referer, mtpid') acc)

updateConst
  :: Referer -> TypeAssertions -> TypeAssertionCache -> TypeAssertionCache
updateConst referer newTas tac = updateDataPoint referer Nothing newTas tac

removeConst :: Referer -> TypeAssertionCache -> TypeAssertionCache
removeConst referer tac = updateConst referer mempty tac

updateTp
  :: Referer -> TpId -> TypeAssertions -> TypeAssertionCache
  -> TypeAssertionCache
updateTp referer tpid newTas tac =
  updateDataPoint referer (Just tpid) newTas tac

removeTp :: Referer -> TpId -> TypeAssertionCache -> TypeAssertionCache
removeTp referer tpid tac = updateTp referer tpid mempty tac

lookup :: Referee -> TypeAssertionCache -> Map Referer TypeAssertion
lookup referee (TypeAssertionCache _ trrs trds) =
  case Map.lookup referee trds of
    Nothing -> mempty
    Just dn -> Map.fromList
      $ fmap ((,TypeAssertion (unReferee referee) dn) . fst)
      $ Set.toList $ Mos.lookup referee trrs

referers :: Referee -> TypeAssertionCache -> Set (Referer, Maybe TpId)
referers referee = Mos.lookup referee . tacRefereeReferers
