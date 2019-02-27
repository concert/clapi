{-# LANGUAGE
    DataKinds
  , GADTs
  , LambdaCase
#-}

-- | Valuspace maniupulations common to both handleTrpd and handleTrcud
module Clapi.Valuespace.Common where

import Control.Lens (modifying)
import Control.Monad (unless, void)
import Control.Monad.Except (throwError)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Set (Set)

import qualified Clapi.Tree as Tree
import Clapi.Types.Base (TpId, Attributee, TypeEnumOf(..))
import Clapi.Types.Digests (TimeSeriesDataOp(..), DataChange(..))
import Clapi.Types.Definitions
  (Definition(..), SomeDefinition(..), MetaType(..))
import Clapi.Types.Error (ErrsT, collect, eitherModifying, eitherThrow)
import Clapi.Types.Path (Path)
import Clapi.Types.Wire (SomeWireValue)
import Clapi.Validator (TypeAssertion(..), validateValues)

import Clapi.Internal.Valuespace (Valuespace, vsTree, vsTac)
import Clapi.Valuespace.Errors
  (AccessError, ErrorString(..), StructuralError(..), ValidationError(..))
import Clapi.Valuespace.ErrWrap (Errs, Wraps(..), throw)
import Clapi.Valuespace.Prim
  ( VsM', pathDefName, pathDef
  , pathError, pathErrors, tpErrors, castSingleErr)
import qualified Clapi.Valuespace.Xrefs as Xrefs



updatePathData
  :: ( Errs '[StructuralError, ValidationError, ErrorString, AccessError] e
     , Monad m)
  => Path -> DataChange -> VsM' e m ()
updatePathData p dc = do
    SomeDefinition def <- pathError p $ pathDef p
    case def of
      TupleDef {tupDefILimit = ilimit } -> case ilimit of
        Nothing -> applyConstChange def dc
        _ -> applyTsChanges def dc
      _ -> pathError p $ throw UnexpectedNodeType
  where
    applyConstChange
      :: ( Errs '[StructuralError, ValidationError, ErrorString, AccessError] e
         , Monad m)
      => Definition 'Tuple -> DataChange -> VsM' e m ()
    applyConstChange tdef = \case
      TimeChange {} -> pathError p $ throw TsChangeOnConst
      ConstChange att wvs -> pathErrors p $ do
        tyAsserts <- validateTupleValues tdef wvs
        modifying vsTree $ Tree.constSetAt att p wvs
        modifying vsTac $ Xrefs.updateConst (Xrefs.Referer p) tyAsserts

    applyTsChanges
      :: ( Errs '[AccessError, ErrorString, StructuralError, ValidationError] e
         , Monad m)
      => Definition 'Tuple -> DataChange -> VsM' e m ()
    applyTsChanges tdef = \case
      TimeChange m -> void $ collect $ Map.mapWithKey (applyTpChange tdef) m
      ConstChange {} -> pathError p $ throw ConstChangeOnTs

    applyTpChange
      :: (Errs '[AccessError, ErrorString, ValidationError] e, Monad m)
      => Definition 'Tuple -> TpId -> (Maybe Attributee, TimeSeriesDataOp)
      -> VsM' e m ()
    applyTpChange tdef tpid (att, tdOp) = tpErrors p tpid $ case tdOp of
      OpSet t wvs i -> do
        unless (Just (typeEnumOf i) <= tupDefILimit tdef) $ throwError $ wrap
          <$> [BadInterpolationType (typeEnumOf i) (tupDefILimit tdef)]
        tyAsserts <- validateTupleValues tdef wvs
        eitherModifying vsTree $
          first (\s -> [wrap $ ErrorString s]) . Tree.setTpAt att p tpid t wvs i
        modifying vsTac $
          Xrefs.updateTp (Xrefs.Referer p) tpid tyAsserts
      OpRemove -> do
        eitherModifying vsTree $
          first (\s -> [wrap $ ErrorString s]) . Tree.removeTpAt att p tpid
        modifying vsTac $ Xrefs.removeTp (Xrefs.Referer p) tpid

validateTupleValues
  :: (Errs '[ValidationError, ErrorString, AccessError] e, Monad m)
  => Definition 'Tuple -> [SomeWireValue]
  -> ErrsT Valuespace [e] m Xrefs.TypeAssertions
validateTupleValues tdef wvs = do
  tas <- eitherThrow
    $ first (fmap $ wrap . DataValidationError)
    $ validateValues (toList $ tupDefTys tdef) wvs
  checkTypeAssertions tas

checkTypeAssertions
  :: (Errs '[ValidationError, ErrorString, AccessError] e, Monad m)
  => Set TypeAssertion
  -> ErrsT Valuespace [e] m Xrefs.TypeAssertions
checkTypeAssertions tas = mapM_ checkTypeAssertion tas
  >> return (Xrefs.toTypeAssertions tas)

-- FIXME: naming is confusing between this one that grabs the actDn and the
-- actual check which takes actDn:
checkTypeAssertion
  :: (Errs '[ValidationError, ErrorString, AccessError] e, Monad m)
  => TypeAssertion -> ErrsT Valuespace [e] m ()
checkTypeAssertion (TypeAssertion path expDn) = castSingleErr $ do
    actDn <- pathDefName path
    unless (actDn == expDn) $ throw $ XRefError expDn actDn
