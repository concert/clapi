{-# LANGUAGE
    LambdaCase
#-}

module Clapi.Valuespace.Xrefs
  -- ( References(..), getReferees
  -- , Xrefs, xrefsFwd, xrefsRev, empty
  -- , update, updateLookup, lookupFwd, lookupRev
  -- ) where
  where


import Data.Bifunctor (bimap, first)
import Data.Foldable (foldl', toList)
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

data References
  = NoReferences
  | ConstReferences (Set Referee)
  | TsReferences (Mos TpId Referee)
  deriving Show

getReferees :: References -> [(Maybe TpId, Referee)]
getReferees = \case
  NoReferences -> []
  ConstReferences referees -> (Nothing,) <$> Set.toList referees
  TsReferences referees -> first Just <$> Mos.toList referees

toReferences :: Maybe (Either (Set Referee) (Mos TpId Referee)) -> References
toReferences = maybe NoReferences $ either ConstReferences TsReferences


data Xrefs
  = Xrefs
  { xrefsFwd :: Map Referer (Either (Set Referee) (Mos TpId Referee))
  , xrefsRev :: Mos Referee (Maybe TpId, Referer)
  } deriving Show


empty :: Xrefs
empty = Xrefs mempty mempty

updateLookup :: Referer -> References -> Xrefs -> (References, Xrefs)
updateLookup referer newReferences (Xrefs fwd rev) =
  let
    (oldReferences, fwd') = first toReferences $ Map.alterF
      (\mr -> (mr, case newReferences of
          NoReferences -> Nothing
          ConstReferences referees -> Just (Left referees)
          TsReferences referees -> Just (Right referees)))
      referer fwd
    rev' =
      foldMos Mos.insert mempty newReferences <>
      foldMos Mos.delete rev oldReferences
  in
    (oldReferences, Xrefs fwd' rev')
  where
    foldMos
      :: (Referee -> (Maybe TpId, Referer) -> c -> c) -> c -> References -> c
    foldMos f mos =
      foldl' (\acc (mtpid, referee) -> f referee (mtpid, referer) acc) mos
      . getReferees

update :: Referer -> References -> Xrefs -> Xrefs
update referer newReferences = snd . updateLookup referer newReferences

updateConst :: Referer -> Set Referee -> Xrefs -> Xrefs
updateConst = undefined

updateTs :: Referer -> TpId -> Set Referee -> Xrefs -> Xrefs
updateTs = undefined

rmTp :: Referer -> TpId -> Xrefs -> Xrefs
rmTp = undefined

lookupFwd :: Referer -> Xrefs -> References
lookupFwd referer (Xrefs fwd _) = toReferences $ Map.lookup referer fwd

lookupRev :: Referee -> Xrefs -> Set (Maybe TpId, Referer)
lookupRev referee (Xrefs _ rev) = Mos.lookup referee rev

-- Attempt Number4(!):

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


emptyTac :: TypeAssertionCache
emptyTac = TypeAssertionCache mempty mempty mempty

gubbins
  :: Referer -> Maybe TpId -> TypeAssertions -> TypeAssertionCache
  -> TypeAssertionCache
gubbins referer mtpid newTas tac =
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

updateConstTas
  :: Referer -> TypeAssertions -> TypeAssertionCache -> TypeAssertionCache
updateConstTas referer newTas tac = gubbins referer Nothing newTas tac

removeTas :: Referer -> TypeAssertionCache -> TypeAssertionCache
removeTas referer tac = updateConstTas referer mempty tac

updateTpTas
  :: Referer -> TpId -> TypeAssertions -> TypeAssertionCache
  -> TypeAssertionCache
updateTpTas referer tpid newTas tac = gubbins referer (Just tpid) newTas tac

removeTpTas :: Referer -> TpId -> TypeAssertionCache -> TypeAssertionCache
removeTpTas referer tpid tac = updateTpTas referer tpid mempty tac

lookup :: Referee -> TypeAssertionCache -> Map Referer TypeAssertion
lookup referee (TypeAssertionCache _ trrs trds) =
  case Map.lookup referee trds of
    Nothing -> mempty
    Just dn -> Map.fromList
      $ fmap ((,TypeAssertion (unReferee referee) dn) . fst)
      $ Set.toList $ Mos.lookup referee trrs

referers :: Referee -> TypeAssertionCache -> Set (Referer, Maybe TpId)
referers referee = Mos.lookup referee . tacRefereeReferers
