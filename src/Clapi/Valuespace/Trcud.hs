{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , GADTs
#-}

module Clapi.Valuespace.Trcud
  ( processTrcud
  ) where

import Control.Monad (join, unless, void)
import Control.Monad.Except (MonadError, throwError)
import Data.Bifunctor (bimap, first)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Mol (Mol)
import Data.Map.Mos (Mos(..))
import qualified Data.Map.Mos as Mos

import Clapi.Types.AssocList (AssocList)
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Base (Attributee)
import Clapi.Types.Definitions
  ( Definition(..), SomeDefinition(..), PostDefinition(..), Editability(..))
import Clapi.Types.Digests
  ( DataErrorIndex, ContOps, OrderedContOps, DataChange, Creates, CreateOp(..)
  , DataDigest
  , TrDigest(..), TrcUpdateDigest, FrDigest(..), FrpDigest, frpdEmpty)
import Clapi.Types.Error (ErrsT, eitherThrow)
import qualified Clapi.Types.Error as Error
import Clapi.Types.SequenceOps
  ( SequenceOp(..), unDependencyOrdered, dependencyOrder')
import Clapi.Types.Name (DataName, Placeholder)
import Clapi.Types.Path (Path)
import Clapi.Util (strictZipWith, fmtStrictZipError)
import Clapi.Validator (TypeAssertion, validateValues)

import Clapi.Internal.Valuespace (Valuespace, EPS)
import Clapi.Valuespace.Common (updatePathData, checkTypeAssertions)
import Clapi.Valuespace.Errors
  ( AccessError(..), ConsumerError(..), ErrorString(..)
  , SeqOpError(..)  , StructuralError(..), ValidationError(..)
  , ConsumerDependencyError)
import Clapi.Valuespace.ErrWrap (Errs, Wraps(..), throw, liftExcept)
import Clapi.Valuespace.Prim
  ( VsM', pathChildren, pathDef, pathPostDef, pathExists, pathEditability
  , pathError, pathErrors)


processTrcud
  :: Monad m
  => TrcUpdateDigest -> Valuespace
  -> m (Mol DataErrorIndex ConsumerError, FrpDigest)
processTrcud trcud vs = fst
  <$> Error.softRunErrsT (frpdEmpty $ trcudNs trcud) (processTrcud_ trcud) vs

processTrcud_ :: Monad m => TrcUpdateDigest -> VsM' ConsumerError m FrpDigest
processTrcud_ (Trcud ns dat crs cops) = do
  (newPhs, crs') <- guardCreates crs
  dat' <- guardClientUpdates dat
  orderedCops <- guardClientCops newPhs cops
  return $ Frpd ns dat' crs' orderedCops

type PathCreates = Map Placeholder (Maybe Attributee, CreateOp)

guardCreates
  :: ( Errs '[AccessError, ConsumerError, ErrorString, ValidationError] e
     , Monad m)
  => Creates -> VsM' e m (Mos Path Placeholder, Creates)
guardCreates = fmap output . Error.filterErrs . Map.mapWithKey perPath
  where
    perPath
      :: ( Errs '[AccessError, ConsumerError, ErrorString, ValidationError] e
         , Monad m)
      => Path -> PathCreates -> VsM' e m PathCreates
    perPath p m = do
      pd <- pathError p $ pathPostDef p
      pathErrors p $ validateCreates p pd m

    output :: Creates -> (Mos Path Placeholder, Creates)
    output m = (Mos.fromMap $ Map.keysSet <$> m, m)

guardReadOnly
  :: (Errs '[roErr, ErrorString, AccessError] e, Monad m)
  => roErr -> Path -> VsM' e m ()
guardReadOnly roErr p = pathError p $ pathEditability p >>= \case
  ReadOnly -> throw roErr
  Editable -> return ()

guardClientUpdates
  :: ( Errs
       '[ AccessError, ConsumerError, ErrorString, StructuralError
        , ValidationError] e
     , Monad m)
  => DataDigest -> VsM' e m DataDigest
guardClientUpdates = Error.filterErrs . AL.fmapWithKey atPath
  where
    atPath
      :: ( Errs
           '[ AccessError, ConsumerError, ErrorString, StructuralError
            , ValidationError] e
         , Monad m)
      => Path -> DataChange -> VsM' e m DataChange
    atPath p dc = do
      guardReadOnly ReadOnlyEdit p
      exists <- pathError p $ pathExists p
      if exists
        then updatePathData p dc
        else pathError p $ throw NodeNotFound
      return dc

guardClientCops
  :: ( Errs
       '[ SeqOpError EPS, ErrorString, AccessError, ConsumerError
        , StructuralError, ConsumerDependencyError] e
     , Monad m)
  => Mos Path Placeholder -> ContOps EPS -> VsM' e m (OrderedContOps EPS)
guardClientCops pphs = Error.filterErrs . Map.mapWithKey perPath
  where
    perPath
      :: (Errs
          '[ ErrorString, AccessError, ConsumerError, SeqOpError EPS
           , StructuralError, ConsumerDependencyError] e
         , Monad m)
      => Path -> Map EPS (x, SequenceOp EPS)
      -> VsM' e m (AssocList EPS (x, SequenceOp EPS))
    perPath p m = do
      guardReadOnly ReadOnlySeqOps p
      SomeDefinition def <- pathError p $ pathDef p
      case def of
        ArrayDef {} -> do
          kids <- pathError p $ Set.fromList <$> pathChildren p
          pathErrors p $ doFilter (validateCop kids $ Mos.lookup p pphs) m
          pathError p $ unDependencyOrdered
            <$> liftExcept (dependencyOrder' snd m)
        _ -> pathError p $ throw $ SeqOpsOnNonArray

    -- FIXME: it would be nice to re-use this validation for the Provider
    -- checking too:
    validateCop
      :: (Wraps (SeqOpError EPS) e, MonadError [e] m)
      => Set DataName -> Set Placeholder -> EPS -> (x, SequenceOp EPS) -> m ()
    validateCop kids phs kidToChange (_, so) =
      let allKids = Set.mapMonotonic Right kids <> Set.mapMonotonic Left phs in
      case so of
        SoAfter (Just t) -> do
          unless (kidToChange `Set.member` allKids) $
            throwError $ wrap <$> [SeqOpMovedMissingChild @EPS kidToChange]
          unless (t `Set.member` allKids) $
            throwError $ wrap <$> [SeqOpTargetMissing @EPS kidToChange t]
        -- It doesn't matter if the client removed something that's already
        -- gone:
        _ -> return ()


validateCreates
  :: ( Errs '[AccessError, ConsumerError, ErrorString, ValidationError] e
     , Monad m)
  => Path -> PostDefinition -> PathCreates -> ErrsT Valuespace [e] m PathCreates
validateCreates p pdef = Error.filterErrs . Map.mapWithKey
  (\_ph x@(_att, cr) -> validateCreateValues pdef cr >> return x)

validateCreateValues
  :: (Errs '[AccessError, ErrorString, ValidationError] e, Monad m)
  => PostDefinition -> CreateOp -> ErrsT Valuespace [e] m ()
validateCreateValues pdef cr = do
    tas <- eitherThrow $ first (fmap $ wrap . DataValidationError) $ combine
      $ fmtStrictZipError "post def arg types" "list of wire values"
      $ strictZipWith validateValues (toList $ postDefArgs pdef) (ocArgs cr)
    void $ checkTypeAssertions tas
  where
    collectErrors
      -- FIXME: I feel like I've written this kind of utility before...
      :: [Either [String] (Set TypeAssertion)]
      -> Either [String] (Set TypeAssertion)
    collectErrors x =
      let
        (errs, tas) = foldMap (either (,mempty) (mempty,)) x
      in
        if null errs then Right tas else Left errs

    combine
      :: Either String [Either [String] (Set TypeAssertion)]
      -> Either [String] (Set TypeAssertion)
    combine = join . bimap pure collectErrors

doFilter
    :: (Monoid e, Monad m)
    => (k -> a -> ErrsT s e m b) -> Map k a -> ErrsT s e m (Map k a)
doFilter f = Error.filterErrs . Map.mapWithKey (\k a -> f k a >> return a)


-- FIXME: Some of this needs to be salvaged even though we no longer have after
-- in creates
-- FIXME: Rename?
-- sortOutAfterDeps
--   :: (Errs '[ConsumerError] e, Monad m)
--   => Set DataName -> PathCreates -> ErrsT s [e] m PathCreates
-- sortOutAfterDeps existingKids =
--       filterDuplicateTargets >=> filterCycles >=> filterMissingNames
--   where
--     filterDuplicateTargets
--       :: (Errs '[ConsumerError] e, MonadWriter [e] m)
--       => PathCreates -> m PathCreates
--     filterDuplicateTargets pCrs =
--       let
--         duplicates :: Map (Maybe EPS) (Set Placeholder)
--         duplicates = Map.filter ((> 1) . Set.size) $ unMos $
--           Mos.invertMap $ ocAfter . snd <$> pCrs
--       in do
--         _ <- sequence $ Map.mapWithKey
--           (\targ phs -> tell [wrap $ MultipleCreatesReferencedTarget phs targ])
--           duplicates
--         return $ pCrs `Map.withoutKeys` fold duplicates

--     filterCycles
--       :: (Errs '[ConsumerError] e, MonadWriter [e] m)
--       => PathCreates -> m PathCreates
--     filterCycles pCrs =
--       let
--         badEdges = detectCycles . Set.fromList
--           . Map.toList . lefts . justs $ ocAfter . snd <$> pCrs
--         cycles = reverse
--           . (\(from, _to) -> followCycle [from] from from)
--           <$> badEdges
--         followCycle acc ph stopAt = case Map.lookup ph pCrs of
--           Just (_, OpCreate _ (Just (Left ph'))) ->
--             if ph' == stopAt then acc else followCycle (ph' : acc) ph' stopAt
--           -- Only PHs should be able to form cycles, so we should never hit this
--           -- case:
--           _ -> acc
--         badPhs = mconcat $ Set.fromList <$> cycles
--       in do
--         mapM_ (tell . pure . wrap . CyclicReferencesInCreates) cycles
--         return $ pCrs `Map.withoutKeys` badPhs

--     partitionOnAfter
--       :: (Maybe EPS -> Bool) -> PathCreates -> (PathCreates, PathCreates)
--     partitionOnAfter f = Map.partition (f . ocAfter . snd)

--     filterMissingNames
--       :: (Errs '[ConsumerError] e, MonadWriter [e] m)
--       => PathCreates -> m PathCreates
--     filterMissingNames pCrs =
--       let
--         (roots, dependants) = partitionOnAfter
--           (maybe True (either (`Map.notMember` pCrs) (const True))) pCrs
--         (validRoots, invalidRoots) = partitionOnAfter
--           (maybe True (either (const False) (`Set.member` existingKids))) roots
--       in do
--         _ <- sequence $ Map.mapWithKey
--           (\ph (_, crop) -> tell $ maybe
--             []  -- `Nothing` is always a valid root
--             (pure . wrap . MissingCreatePositionTarget ph) $
--             ocAfter crop)
--           invalidRoots
--         return $ validRoots <> dependants
