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
import Control.Applicative ((<|>))
import Control.Monad (when, unless, void)
import Control.Monad.Fail (MonadFail(..))
import Data.Either (isLeft, fromLeft, fromRight)
import Data.Int
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word

import qualified Data.Map.Mos as Mos

import Data.Maybe.Clapi (note)

import Clapi.TH
import Clapi.Util
  (strictZipWith, fmtStrictZipError, partitionDifference, showItems)
import Clapi.Tree (RoseTree(..), RoseTreeNode, treeInsert)
import qualified Clapi.Tree as Tree
import Clapi.Types (WireValue(..))
import Clapi.Types.Base (InterpolationLimit(ILUninterpolated))
import Clapi.Types.AssocList
  ( alKeysSet, alValues, alLookup, alFromMap, alSingleton, alCons
  , unsafeMkAssocList, alMapKeys)
import Clapi.Types.Definitions
  ( Definition(..), Liberty(..), TupleDefinition(..)
  , StructDefinition(..), ArrayDefinition(..), defDispatch, childLibertyFor)
import Clapi.Types.Digests
  (DefOp(..), ContainerOps, DataDigest, TrpDigest(..))
import Clapi.Types.Messages (ErrorIndex(..))
import Clapi.Types.Path
  (Seg, Path, pattern (:/), pattern Root, pattern (:</), TypeName(..))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree (TreeType(..), ttWord32, ttInt32, unbounded)
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

apiNs :: Seg
apiNs = [segq|api|]

rootTypeName, apiTypeName :: TypeName
rootTypeName = TypeName apiNs [segq|root|]
apiTypeName = TypeName apiNs apiNs

apiDef :: StructDefinition
apiDef = StructDefinition "Information about CLAPI itself" $
  alSingleton [segq|version|] (TypeName apiNs [segq|version|], Cannot)

versionDef :: TupleDefinition
versionDef = TupleDefinition "The version of CLAPI" (unsafeMkAssocList
  [ ([segq|major|], ttWord32 unbounded)
  , ([segq|minor|], ttWord32 unbounded)
  , ([segq|revision|], ttInt32 unbounded)
  ]) ILUninterpolated

rootDefAddChild :: MonadFail m => Seg -> StructDefinition -> m StructDefinition
rootDefAddChild ns sd = do
  newTys <- alCons ns (TypeName ns ns, Cannot) $ strDefTypes sd
  return $ sd {strDefTypes = newTys}

baseValuespace :: Valuespace
baseValuespace = Valuespace baseTree baseDefs baseTas
  where
    vseg = [segq|version|]
    version = RtConstData Nothing
      [WireValue @Word32 0, WireValue @Word32 1, WireValue @Int32 (-1023)]
    baseTree =
      treeInsert Nothing (Root :/ apiNs :/ vseg) version Tree.RtEmpty
    baseDefs = Map.singleton apiNs $ Map.fromList
      [ (apiNs, StructDef apiDef)
      , (vseg, TupleDef versionDef)
      ]
    baseTas = Mos.dependenciesFromMap $ Map.fromList
      [ (Root, rootTypeName)
      , (Root :/ apiNs, apiTypeName)
      , (Root :/ apiNs :/ vseg, TypeName apiNs vseg)
      ]

getTypeAssignment :: MonadFail m => DefMap -> Path -> m TypeName
getTypeAssignment defs thePath = lookupDef rootTypeName defs >>= go thePath
  where
    go path def = case path of
      seg :</ p -> do
        tn <- tnForChild def seg
        def' <- lookupDef tn defs
        case p of
          Root -> return tn
          _ -> go p def'
      _ -> return rootTypeName
    tnForChild def seg = case def of
      TupleDef _ -> fail "Tuples have no children"
      StructDef (StructDefinition _ tal) -> fst <$> alLookup seg tal
      ArrayDef (ArrayDefinition _ tn _) -> return tn

lookupDef :: MonadFail m => TypeName -> DefMap -> m Definition
lookupDef tn@(TypeName ns s) defs = note "Missing def" $
    (Map.lookup ns defs >>= Map.lookup s) <|>
    if tn == rootTypeName then Just rootDef else Nothing
  where
    -- NB: We generate the root def on the fly when people ask about it
    rootDef = StructDef $ StructDefinition "root def doc" $ alFromMap $
      Map.mapWithKey (\k _ -> (TypeName k k, Cannot)) defs

vsLookupDef :: MonadFail m => TypeName -> Valuespace -> m Definition
vsLookupDef tn (Valuespace _ defs _) = lookupDef tn defs

lookupTypeName :: MonadFail m => Path -> TypeAssignmentMap -> m TypeName
lookupTypeName p = note "Type name not found" . Mos.getDependency p

defForPath :: MonadFail m => Path -> Valuespace -> m Definition
defForPath p (Valuespace _ defs tas) =
  lookupTypeName p tas >>= flip lookupDef defs

getLiberty :: MonadFail m => Path -> Valuespace -> m Liberty
getLiberty path vs = case path of
  Root :/ _ -> return Cannot
  p :/ s -> defForPath p vs >>= defDispatch (flip childLibertyFor s)
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
  :: Ord a => TrpDigest -> Valuespace
  -> Either (Map (ErrorIndex a) [Text]) Valuespace
processToRelayProviderDigest trpd vs =
  let
    ns = trpdNamespace trpd
    qData = fromJust $ alMapKeys (ns :</) $ trpdData trpd
    qDefs = Map.mapKeys (TypeName ns) $ trpdDefinitions trpd
    qCops = Map.mapKeys (ns :</) $ trpdContainerOps trpd
    (undefOps, defOps) = Map.partition isUndef (trpdDefinitions trpd)
    defs' =
      let
        newDefs = odDef <$> defOps
        updateNsDefs Nothing = Just newDefs
        updateNsDefs (Just existingDefs) = Just $ Map.union (odDef <$> defOps) $
          Map.withoutKeys existingDefs (Map.keysSet undefOps)
      in
        Map.alter updateNsDefs ns (vsTyDefs vs)
    -- FIXME: do I need to sneak root in here?
    possiblyChangedTypePaths = mconcat $
      flip Mos.getDependants (vsTyAssns vs) <$> Map.keys qDefs
    -- ftam: the type assignments we know can't have changed
    ftam = foldl removeTamSubtree (vsTyAssns vs) possiblyChangedTypePaths
    dataPathsRequiringValidation = Set.union
      (alKeysSet qData) possiblyChangedTypePaths
    (updateErrs, tree') = Tree.updateTreeWithDigest qCops qData (vsTree vs)
    -- FIXME: creating a set of all the paths in the tree might not be the
    -- best idea...
    (pathsOfUnknownType, missingPaths) =
          (Set.fromList $ Tree.treePaths Root tree')
          `partitionDifference`
          Mos.allDependants ftam
  in do
    unless (null updateErrs) $ Left $ Map.mapKeys PathError updateErrs
    -- Fill in ones from pathsOfUnknownType with function that works type from defs
    newTaMap <- mapFromSetWErr (getTypeAssignment defs') pathsOfUnknownType
    let tam' = Mos.dependenciesFromMap $ Map.union newTaMap $ fst ftam
    let vs' = Valuespace tree' defs' tam'
    unless (null missingPaths) $ Left $ Map.mapKeys PathError $
      Map.fromSet (const $ ["missing"]) missingPaths
    _ <- mapFromSetWErr (validatePath vs') dataPathsRequiringValidation
    return vs'
  where
    isUndef :: DefOp -> Bool
    isUndef OpUndefine = True
    isUndef _ = False
    fromLeft' = fromLeft $ error "expecting Left"
    fromRight' = fromRight $ error "expecting Right"
    mapFromSetWErr
      :: (Ord a)
      => (Path -> Either String y) -> Set Path
      -> Either (Map (ErrorIndex a) [Text]) (Map Path y)
    mapFromSetWErr f s =
      let (lmap, rmap) = Map.partition isLeft $ Map.fromSet f s in
        if null lmap
          then return $ fromRight' <$> rmap
          else Left $ Map.mapKeys PathError $
               pure . Text.pack . fromLeft' <$> lmap

processToRelayClientDigest
  :: ContainerOps -> DataDigest -> Valuespace -> Map Path [Text]
processToRelayClientDigest reords dd vs =
  let
    (updateErrs, tree') = Tree.updateTreeWithDigest reords dd (vsTree vs)
    vs' = vs {vsTree = tree'}
    touchedPaths = Set.union (alKeysSet dd) (Map.keysSet reords)
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
      let
        defKeys = alKeysSet alDef
        contKeys = alKeysSet alCont
        (missing, added) = partitionDifference defKeys contKeys
        missingStr = if null missing then ""
          else " missing " ++ showItems (Set.toList missing)
        addedStr = if null added then ""
          else " extraneous " ++ showItems (Set.toList added)
      in
        when (defKeys /= contKeys) $ fail $ "bad struct keys:" ++
          intercalate "; " (filter (not . null) [missingStr, addedStr])
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
