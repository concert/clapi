{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
#-}
module Clapi.Valuespace.Prim
  ( lookupDef, lookupPostDef
  , pathTyInfo, pathDefName, pathEditability
  , pathDef, pathPostDef, pathNode, pathExists, pathChildren
  , Valuespace, baseValuespace

  , VsM', pathError, pathErrors, tpErrors, castVsMError, castSingleErr
  ) where

import Control.Lens (use)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Trans (lift)
import qualified Data.Map as Map

import qualified Data.Map.Dependencies as Dependencies
import Data.Map.Mol (Mol)
import qualified Data.Map.Mol as Mol

import Clapi.Tree (RoseTreeNode(..))
import qualified Clapi.Tree as Tree
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Base (TpId)
import Clapi.Types.Definitions
  ( Definition(..), SomeDefinition(..), PostDefinition(..)
  , Editability, getTyInfoForName, structDef)
import Clapi.Types.Digests (DataErrorIndex(..))
import Clapi.Types.Error (ErrsT, castErrs)
import Clapi.Types.Name (DataName, DefName, PostDefName)
import Clapi.Types.Path (Path, pattern Root, pattern (:</))
import Clapi.Types.UniqList (unUniqList)
import Clapi.Types.Wire (SomeWireValue)
import Clapi.Valuespace.Errors
  ( AccessError(..), ConsumerError(..), ErrorString(..))
import Clapi.Valuespace.ErrWrap (MonadErrors, note, throw, liftEither)
import qualified Clapi.Valuespace.Xrefs as Xrefs

import Clapi.Internal.Valuespace
  ( Valuespace(..), vsTree, vsRootDefName, vsRootEditability, vsTyDefs
  , vsPostDefs)


lookupDef
  :: (MonadState Valuespace m, MonadErrors '[AccessError] e m)
  => DefName -> m SomeDefinition
lookupDef dn = use vsTyDefs >>= note (DefNotFound dn) . Map.lookup dn

lookupPostDef
  :: (MonadState Valuespace m, MonadErrors '[AccessError] e m)
  => PostDefName -> m PostDefinition
lookupPostDef dn = use vsPostDefs >>= note (PostDefNotFound dn) . Map.lookup dn

pathTyInfo
  :: (MonadState Valuespace m, MonadErrors '[AccessError, ErrorString] e m)
  => Path -> m (DefName, Editability)
pathTyInfo path = do
    dn <- use vsRootDefName
    ed <- use vsRootEditability
    go path (dn, ed)
  where
    go
      :: (MonadState Valuespace m, MonadErrors '[AccessError, ErrorString] e m)
      => Path -> (DefName, Editability) -> m (DefName, Editability)
    go (n :</ p) (dn, _) = do
      SomeDefinition def <- lookupDef dn
      r <- liftEither @ErrorString $ getTyInfoForName n def
      go p r
    go _ r = return r

pathDefName
  :: (MonadState Valuespace m, MonadErrors '[AccessError, ErrorString] e m)
  => Path -> m DefName
pathDefName path = fst <$> pathTyInfo path

pathEditability
  :: (MonadState Valuespace m, MonadErrors '[AccessError, ErrorString] e m)
  => Path -> m Editability
pathEditability path = snd <$> pathTyInfo path

pathDef
  :: (MonadState Valuespace m, MonadErrors '[AccessError, ErrorString] e m)
  => Path -> m SomeDefinition
pathDef path = pathDefName path >>= lookupDef

pathPostDef
  :: ( MonadState Valuespace m
     , MonadErrors '[AccessError, ConsumerError, ErrorString] e m)
  => Path -> m PostDefinition
pathPostDef path = do
  SomeDefinition def <- pathDef path
  case def of
    ArrayDef { arrDefPostTy = mpd } -> maybe
      (throw CreatesNotSupported) lookupPostDef mpd
    -- FIXME: Might be better to have have a more specific error type here...
    _ -> throw CreatesNotSupported

pathNode
  :: (MonadState Valuespace m, MonadErrors '[AccessError] e m)
  => Path -> m (RoseTreeNode [SomeWireValue])
pathNode path = use vsTree >>= guard . Tree.lookupNode path
  where
    guard Nothing = throw NodeNotFound
    guard (Just n) = case n of
      -- FIXME: More evidence for getting rid of RtnEmpty?
      RtnEmpty -> throw NodeNotFound
      _ -> return n

pathExists :: MonadState Valuespace m => Path -> m Bool
pathExists path = use vsTree >>= return . go . Tree.lookupNode path
  where
    go Nothing = False
    go (Just RtnEmpty) = False
    go _ = True

pathChildren
  :: (MonadState Valuespace m, MonadErrors '[AccessError] e m)
  => Path -> m [DataName]
pathChildren path = pathNode path >>= return . \case
  RtnChildren al -> unUniqList $ AL.keys al
  _ -> mempty


baseValuespace :: DefName -> Editability -> Valuespace
baseValuespace rootDn rootEd = Valuespace
    (Tree.RtContainer mempty)
    mempty
    (Map.singleton rootDn emptyStructDef)
    rootDn
    rootEd
    (Dependencies.singleton Root rootDn)
    Xrefs.empty
  where
    emptyStructDef = structDef "Empty namespace" mempty


-- Some basic type and error handling stuff that might want to live elsewhere:

type VsM e m = ExceptT e (StateT Valuespace m)
type VsM' e m = ErrsT Valuespace (Mol DataErrorIndex e) m

castVsMError :: Monad m => DataErrorIndex -> VsM e m a -> VsM' e m a
castVsMError idx m =
  (lift $ lift $ runExceptT m) >>=
  either (throwError . Mol.singleton idx) return

castSingleErr
  :: Monad m => VsM e m a -> ErrsT Valuespace [e] m a
castSingleErr m =
  (lift $ lift $ runExceptT m) >>= either (throwError . pure) return

pathError :: Monad m => Path -> VsM e m a -> VsM' e m a
pathError p = castVsMError $ PathError p

pathErrors
  :: Monad m
  => Path -> ErrsT Valuespace [e] m a
  -> ErrsT Valuespace (Mol DataErrorIndex e) m a
pathErrors p = castErrs $ Mol.singletonList $ PathError p

tpErrors
  :: Monad m
  => Path -> TpId -> ErrsT Valuespace [e] m a
  -> ErrsT Valuespace (Mol DataErrorIndex e) m a
tpErrors p tpid = castErrs $ Mol.singletonList $ TimePointError p tpid
