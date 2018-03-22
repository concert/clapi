{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

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

import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Data.Maybe.Clapi (note)

import qualified Data.Map.Mol as Mol
import Clapi.TH
import Clapi.Util
  (strictZipWith, fmtStrictZipError, partitionDifference, showItems)
import Clapi.Tree (RoseTree(..), RoseTreeNode, treeInsert, treeChildren, TpId)
import qualified Clapi.Tree as Tree
import Clapi.Types (WireValue(..))
import Clapi.Types.Base (InterpolationLimit(ILUninterpolated))
import Clapi.Types.AssocList
  ( alKeysSet, alValues, alLookup, alFromMap, alSingleton, alCons
  , unsafeMkAssocList, alMapKeys, unAssocList)
import Clapi.Types.Definitions
  ( Definition(..), Liberty(..), TupleDefinition(..)
  , StructDefinition(..), ArrayDefinition(..), defDispatch, childLibertyFor
  , childTypeFor)
import Clapi.Types.Digests
  (DefOp(..), ContainerOps, DataDigest, TrpDigest(..))
import Clapi.Types.Messages (ErrorIndex(..))
import Clapi.Types.Path
  (Seg, Path, pattern (:/), pattern Root, pattern (:</), TypeName(..))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree (TreeType(..), ttWord32, ttInt32, unbounded)
import Clapi.Validator (validate, extractTypeAssertion)
import qualified Clapi.Types.Dkmap as Dkmap

type TypeAssignmentMap = Mos.Dependencies Path TypeName
type DefMap = Map Seg (Map Seg Definition)

data Valuespace = Valuespace
  { vsTree :: RoseTree [WireValue]
  , vsTyDefs :: DefMap
  , vsTyAssns :: TypeAssignmentMap
  } deriving (Eq, Show)

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
    rtn <- note "Path not found" $ Tree.treeLookupNode p tree
    tn <- lookupTypeName p tas
    def <- lookupDef tn defs
    lib <- getLiberty p vs
    return (def, tn, lib, rtn)

validatePath :: MonadFail m => Valuespace -> Path -> m ()
validatePath vs p = undefined -- do
    -- def <- defForPath p vs
    -- t <- note "Missing tree node" $ Tree.treeLookup p $ vsTree vs
    -- validateRoseTree def t

validateTree
  :: Valuespace -> Either (Map (ErrorIndex TypeName) [Text]) Valuespace
validateTree vs@(Valuespace tree defs _) = wrapInEither $ inner Root tree $ TypeName apiNs [segq|root|]
  where
    calcCas = Mos.dependenciesFromMap $
      Map.fromSet (fromJust . getTypeAssignment defs) $ Set.fromList $
      Tree.treePaths Root tree
    wrapInEither v = if null v then Right vs{vsTyAssns = calcCas} else Left v
    stuff p def (s, ct) = case defDispatch (childTypeFor s) def of
      Nothing -> Map.singleton (PathError $ p :/ s) ["Illegal child"]
      Just tn -> inner (p :/ s) ct tn
    failsAsStrings :: [Either String a] -> [Text]
    failsAsStrings mfs = filter (not . Text.null) $ either Text.pack (const "") <$> mfs
    inner p t tn = case lookupDef tn defs of
      Left msg -> Map.singleton (TypeError tn) [Text.pack msg]
      Right def -> case validateRoseTree def t of
        Left msg -> Map.singleton (PathError p) [Text.pack msg]
        Right refs ->
          let
            asErrorIndex = maybe (PathError p) (TimePointError p)
            checkRef refTn refP = do
              dTn <- getTypeAssignment defs refP
              unless (refTn == dTn) $ fail "Ref to bad type"
              maybe (fail "Ref to missing") (const $ return ()) $ Tree.treeLookup refP tree
            refErrs =
              Map.mapKeys asErrorIndex $ Map.filter (not . null) $
              failsAsStrings . fmap (uncurry checkRef) . Mos.toList <$> refs
            childErrs = Mol.unions $ fmap (stuff p def) $ unAssocList $ treeChildren t
          in Map.union childErrs refErrs

processToRelayProviderDigest
  :: TrpDigest -> Valuespace
  -> Either (Map (ErrorIndex TypeName) [Text]) Valuespace
processToRelayProviderDigest trpd vs =
  let
    ns = trpdNamespace trpd
    qData = fromJust $ alMapKeys (ns :</) $ trpdData trpd
    qCops = Map.mapKeys (ns :</) $ trpdContainerOps trpd
    (undefOps, defOps) = Map.partition isUndef (trpdDefinitions trpd)
    defs' =
      let
        newDefs = odDef <$> defOps
        updateNsDefs Nothing = Just newDefs
        updateNsDefs (Just existingDefs) = Just $ Map.union newDefs $
          Map.withoutKeys existingDefs (Map.keysSet undefOps)
      in
        Map.alter updateNsDefs ns (vsTyDefs vs)
    (updateErrs, tree') = Tree.updateTreeWithDigest qCops qData (vsTree vs)
  in do
    unless (null updateErrs) $ Left $ Map.mapKeys PathError updateErrs
    validateTree $ Valuespace tree' defs' $ vsTyAssns vs
  where
    isUndef :: DefOp -> Bool
    isUndef OpUndefine = True
    isUndef _ = False
    fromLeft' = fromLeft $ error "expecting Left"
    fromRight' = fromRight $ error "expecting Right"

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

validateRoseTree :: MonadFail m => Definition -> RoseTree [WireValue] -> m (Map (Maybe TpId) (Mos TypeName Path))
validateRoseTree def t = case t of
  RtEmpty -> fail "Empty node"
  RtConstData _ wvs -> case def of
    TupleDef (TupleDefinition _ alTreeTypes _) ->
      Map.singleton Nothing <$> validateWireValues (alValues alTreeTypes) wvs
    _ -> fail "Unexpected constant value!"
  RtDataSeries m -> case def of
    TupleDef (TupleDefinition _ alTreeTypes _) ->
      sequence $
      fmap (validateWireValues (alValues alTreeTypes) . snd . snd) $
      Map.mapKeys Just $ Dkmap.valueMap m
    _ -> fail "Unexpected time series data!"
  RtContainer alCont -> case def of
    TupleDef _ -> fail "Y'all have a container where you wanted data"
    StructDef (StructDefinition _ alDef) -> return mempty
    ArrayDef _ -> return mempty

validateWireValues :: MonadFail m => [TreeType] -> [WireValue] -> m (Mos TypeName Path)
validateWireValues tts wvs =
    (fmtStrictZipError "types" "values" $ strictZipWith vr tts wvs) >>= sequence >>= return . Mos.fromList . mconcat
  where
    vr tt wv = validate tt wv >> return (extractTypeAssertion tt wv)

-- FIXME: The VS you get back from this can be invalid WRT refs/inter NS types
vsRelinquish :: Seg -> Valuespace -> Valuespace
vsRelinquish ns (Valuespace tree defs tas) = let nsp = Root :/ ns in Valuespace
  (Tree.treeDelete nsp tree)
  (Map.delete ns defs)
  (Mos.filterDeps
   (\p (TypeName ns' _) -> p `Path.isChildOf` nsp || ns == ns') tas)
