{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}

module Clapi.Valuespace where

import Prelude hiding (fail)
import Control.Monad (when, unless, void)
import Control.Monad.Fail (MonadFail(..))
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Map.Mos as Mos

import Data.Maybe.Clapi (note)

import Clapi.Util (strictZipWith, fmtStrictZipError, partitionDifference)
import Clapi.Types (WireValue(..))
import Clapi.Types.Path
  (Seg, Path, pattern (:/), pattern Root, pattern (:</), TypeName(..))
import qualified Clapi.Types.Path as Path
import Clapi.Tree (RoseTree(..), RoseTreeNode)
import qualified Clapi.Tree as Tree
import Clapi.Types.AssocList (alKeys, alValues, alLookup, alFromMap)
import Clapi.Types.Definitions
  ( Definition(..), Liberty(..), TupleDefinition(..)
  , StructDefinition(..), ArrayDefinition(..), defDispatch, childLibertyFor)
import Clapi.Types.Messages
  (DefOp(..), TrpDigest(..), ChildAssignments, DataDigest)
import Clapi.Types.Tree (TreeType(..))
import Clapi.Validator (validate)


type TypeAssignmentMap = Mos.Dependencies Path TypeName
type DefMap = Map Seg (Map Seg Definition)

data Valuespace = Valuespace
  { vsTree :: RoseTree [WireValue]
  , vsTyDefs :: DefMap
  , vsTyAssns :: TypeAssignmentMap
  } deriving Eq

removeTamSubtree :: TypeAssignmentMap -> Path -> TypeAssignmentMap
removeTamSubtree tam p = Mos.filterDependencies (`Path.isChildOf` p) tam

baseValuespace :: Valuespace
baseValuespace = Valuespace Tree.RtEmpty mempty mempty

getTypeAssignment :: MonadFail m => DefMap -> Path -> m TypeName
getTypeAssignment defs thePath = go rootDef thePath
  where
    rootDef = StructDef $ StructDefinition "root def doc" $ alFromMap $
      Map.mapWithKey (\k _ -> (TypeName k k, Cannot)) defs
    getDef (TypeName ns tseg) = maybe (fail "Missing type name") return $
      Map.lookup ns defs >>= Map.lookup tseg
    go def path = case path of
      seg :</ p -> do
        tn <- tnForChild def seg
        def' <- getDef tn
        case p of
          Root -> return tn
          _ -> go def' p
      _ -> fail "Dunno what to do about root"
    tnForChild def seg = case def of
      TupleDef _ -> fail "Tuples have no children"
      StructDef (StructDefinition _ tal) -> fst <$> alLookup seg tal
      ArrayDef (ArrayDefinition _ tn _) -> return tn

lookupDef :: MonadFail m => TypeName -> DefMap -> m Definition
lookupDef (TypeName ns s) defs = note "Missing def" $ Map.lookup ns defs >>= Map.lookup s

vsLookupDef :: MonadFail m => TypeName -> Valuespace -> m Definition
vsLookupDef tn (Valuespace _ defs _) = lookupDef tn defs

lookupTypeName :: MonadFail m => Path -> TypeAssignmentMap -> m TypeName
lookupTypeName p = note "Path not found" . Mos.getDependency p

defForPath :: MonadFail m => Path -> Valuespace -> m Definition
defForPath p (Valuespace _ defs tas) =
  lookupTypeName p tas >>= flip lookupDef defs

getLiberty :: MonadFail m => Path -> Valuespace -> m Liberty
getLiberty p vs = case p of
  Root :/ _ -> return Cannot
  _ :/ s -> defForPath p vs >>= defDispatch (flip childLibertyFor s)
  _ -> return Cannot

valuespaceGet
  :: MonadFail m => Path -> Valuespace
  -> m (Definition, TypeName, Liberty, RoseTreeNode [WireValue])
valuespaceGet p vs@(Valuespace tree defs tas) = do
    tn <- lookupTypeName p tas
    def <- lookupDef tn defs
    rtn <- note "Tree node not found" $ Tree.treeLookupNode p tree
    lib <- getLiberty p vs
    return (def, tn, lib, rtn)

validatePath :: MonadFail m => Valuespace -> Path -> m ()
validatePath vs p = do
    def <- defForPath p vs
    t <- note "Missing tree node" $ Tree.treeLookup p $ vsTree vs
    validateRoseTree def t

processToRelayProviderDigest
  :: MonadFail m => TrpDigest -> Valuespace -> m Valuespace
processToRelayProviderDigest trpd vs =
  let
    ns = trpdNamespace trpd
    (undefs, defs) = Map.partition isUndef (trpdDefinitions trpd)
    defs' = Map.adjust updateDefs ns (vsTyDefs vs)
    updateDefs existingDefs = Map.union (odDef <$> defs) $
      Map.withoutKeys existingDefs (Map.keysSet undefs)
    changedTns = Set.map (TypeName ns) $ Map.keysSet $ trpdDefinitions trpd
    possiblyChangedTypePaths = mconcat $
      flip Mos.getDependants (vsTyAssns vs) <$> Set.toList changedTns
    -- ftam: the type assignments we know can't have changed
    ftam = foldl removeTamSubtree (vsTyAssns vs) possiblyChangedTypePaths
    dataPathsRequiringValidation = Set.union
      (Map.keysSet (trpdData trpd)) possiblyChangedTypePaths
    (updateErrs, tree') = Tree.updateTreeWithDigest
      (trpdChildAssignments trpd) (trpdData trpd) (vsTree vs)
    -- FIXME: creating a set of all the paths in the tree might not be the
    -- best idea...
    (pathsOfUnknownType, missingPaths) =
          (Set.fromList $ Tree.treePaths Root tree')
          `partitionDifference`
          Mos.allDependants ftam
  in do
    unless (null updateErrs) (fail "We threw all your errors away")
    -- Fill in ones from pathsOfUnknownType with function that works type from defs
    newTaMap <- sequence $
      Map.fromSet (getTypeAssignment defs') pathsOfUnknownType
    let tam' = Mos.dependenciesFromMap $ Map.union newTaMap $ fst ftam
    let vs' = Valuespace tree' defs' tam'
    unless (null missingPaths) $ fail "Some expected paths missing"
    mapM_ (validatePath vs') dataPathsRequiringValidation
    return vs'
  where
    isUndef :: DefOp -> Bool
    isUndef OpUndefine = True
    isUndef _ = False

processToRelayClientDigest
  :: ChildAssignments -> DataDigest -> Valuespace -> Map Path [Text]
processToRelayClientDigest cas dd vs =
  let
    (updateErrs, tree') = Tree.updateTreeWithDigest cas dd (vsTree vs)
    vs' = vs {vsTree = tree'}
    touchedPaths = Set.union (Map.keysSet dd) (Map.keysSet cas)
    touchedLiberties = Map.fromSet (flip getLiberty vs) touchedPaths
    cannotErrs = const ["You touched a cannot"]
      <$> Map.filter (== Just Cannot) touchedLiberties
    mustErrs = const ["You failed to provide a value for must"] <$>
      ( Map.filter (== Just Must)
      $ Map.fromSet (flip getLiberty vs) $ Set.fromList
      $ Tree.treeMissing tree')
    validationErrs = Map.fromSet (
      either (return . Text.pack) (const []) . validatePath vs') touchedPaths
  in
    foldl (Map.unionWith (<>)) updateErrs [
      validationErrs, cannotErrs, mustErrs]

validateRoseTree :: MonadFail m => Definition -> RoseTree [WireValue] -> m ()
validateRoseTree def t = case t of
  RtEmpty -> fail "Empty node"
  RtConstData _ wvs -> case def of
    TupleDef (TupleDefinition _ alTreeTypes _) ->
      validateWireValues (alValues alTreeTypes) wvs
    _ -> fail "Unexpected constant value!"
  RtDataSeries m -> case def of
    TupleDef (TupleDefinition _ alTreeTypes _) ->
      mapM_ (validateWireValues (alValues alTreeTypes) . snd . snd) m
    _ -> fail "Unexpected time series data!"
  RtContainer alCont -> case def of
    TupleDef _ -> fail "Y'all have a container where you wanted data"
    StructDef (StructDefinition _ alDef) ->
      when (alKeys alDef /= alKeys alCont) $ fail "bad struct keys"
    ArrayDef _ -> return ()

validateWireValues :: MonadFail m => [TreeType] -> [WireValue] -> m ()
validateWireValues tts wvs =
  (fmtStrictZipError "types" "values" $ strictZipWith validate tts wvs) >>=
  void . sequence

vsRelinquish :: Seg -> Valuespace -> Valuespace
vsRelinquish ns (Valuespace tree defs tas) = let nsp = Root :/ ns in Valuespace
  (Tree.treeDelete nsp tree)
  (Map.delete ns defs)
  (Mos.filterDeps
   (\p (TypeName ns' _) -> p `Path.isChildOf` nsp || ns == ns') tas)
