module Clapi.NamespaceTracker where

-- import Prelude hiding (fail)
-- import Control.Monad.Fail (MonadFail(..))
-- import Control.Monad (forever, when, unless, void)
-- import Control.Monad.State (StateT(..), evalStateT, get, put, modify)
import Control.Monad.Trans (MonadTrans, lift)
-- import Data.Bifunctor (bimap)
-- import Data.Map (Map)
-- import qualified Data.Map as Map
-- import Data.Map.Strict.Merge (merge, zipWithMatched, mapMissing)
-- import Data.Monoid
-- import Data.Set (Set)
-- import qualified Data.Set as Set
-- import Data.Tagged (Tagged)
-- import qualified Data.Text as Text

-- import Data.Map.Mos (Mos)
-- import qualified Data.Map.Mos as Mos

-- import Clapi.Types.AssocList
--   (AssocList, alEmpty,  alFilterKey, alInsert, alFoldlWithKey, alKeysSet)
-- import Clapi.Types.Messages (DataErrorIndex(..))
-- import Clapi.Types.Digests
--   ( TrDigest(..), TrcSubDigest(..), trcsdNamespaces, TrcUpdateDigest(..)
--   , TrpDigest(..), TrprDigest(..)
--   , FrDigest(..), FrcSubDigest(..), FrcUpdateDigest(..)
--   , FrpDigest(..), FrpErrorDigest(..)
--   , DefOp(..), isUndef, frcudNull, frcsdNull, frcsdEmpty
--   , OutboundDigest(..)
--   , OutboundClientInitialisationDigest, OutboundClientUpdateDigest
--   , ClientRegs(..), crNull
--   )
-- -- Also `Either String a` MonadFail instance:
-- import Clapi.Types (Definition, PostDefinition)
-- import Clapi.Types.Path (Path, pattern (:/), pattern Root, Namespace(..), Seg)
-- import Clapi.Types.SequenceOps (isSoAbsent)
-- import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Protocol (Protocol, Directed(..), wait, sendFwd, sendRev)
-- import Clapi.Util (flattenNestedMaps)

-- newtype Originator i = Originator i deriving (Eq, Ord, Show)

-- type ClientGetDigest = ClientRegs

-- data PostNstInboundDigest
--   = PnidRootGet
--   | PnidClientGet ClientGetDigest
--   | PnidTrcud TrcUpdateDigest
--   | PnidTrpd TrpDigest
--   | PnidTrprd TrprDigest
--   deriving (Show)

-- type NstProtocol m i = Protocol
--     (ClientEvent i TrDigest)
--     ((Originator i, PostNstInboundDigest))
--     (Either (Map Namespace i) (ServerEvent i FrDigest))
--     ((Originator i, OutboundDigest))
--     m

-- data NstState i
--   = NstState
--   { nstOwners :: Map Namespace i
--   , nstRegs :: Map i ClientRegs
--   -- FIXME: also tracked in the RelayApi:
--   , nstAllClients :: Set i
--   } deriving Show


-- nstProtocol :: (Monad m, Ord i) => NstProtocol m i ()
-- nstProtocol = evalStateT nstProtocol_ $ NstState mempty mempty mempty

-- nstProtocol_ :: (Monad m, Ord i) => StateT (NstState i) (NstProtocol m i) ()
-- nstProtocol_ = forever $ liftedWaitThen fwd rev
--   where
--     sendFwd' i d = lift $ sendFwd (Originator i, d)
--     fwd (ClientConnect _ i) = do
--       modify $ \nsts -> nsts {nstAllClients = Set.insert i $ nstAllClients nsts}
--       sendFwd' i PnidRootGet
--     fwd (ClientData i trd) =
--       case trd of
--         Trpd d -> claimNamespace i d
--           (throwOutProvider i $ Set.singleton $ trpdNamespace d)
--           (sendFwd' i $ PnidTrpd d)
--         Trprd d -> relinquishNamespace i (trprdNamespace d)
--           (throwOutProvider i $ Set.singleton $ trprdNamespace d)
--           (do
--               unsubscribeFromRelinquishedNs (Set.singleton $ trprdNamespace d)
--                 >>= lift . multicastRev . fmap Frcsd
--               sendFwd' i $ PnidTrprd d
--           )
--         Trcsd d -> guardNsClientSubDigest i d
--           (throwOutProvider i)
--           (do
--               (cgd, frcsd) <- handleSubDigest i d
--               unless (frcsdNull frcsd) $
--                 lift $ sendRev $ Right $ ServerData i $ Frcsd frcsd
--               unless (crNull cgd) $
--                 sendFwd' i $ PnidClientGet cgd
--           )
--         Trcud d -> guardNsClientUpdateDigest i d
--           (throwOutProvider i $ Set.singleton $ trcudNamespace d)
--           (sendFwd' i $ PnidTrcud d)
--     fwd (ClientDisconnect i) = handleDisconnect i
--     rev (Originator i, od) =
--       let send i' = lift . sendRev . Right . ServerData i' in case od of
--         Ocrid frcrd -> send i (Frcrd frcrd)
--         Ocid d -> registerSubs i d >> send i (Frcud d)
--         Ocsed errMap -> send i $ Frcsd $ frcsdEmpty { frcsdErrors = errMap}
--         Ocrd frcrd -> broadcastRev $ Frcrd frcrd
--         -- FIXME: eventually we may collapse the client/time tracking roles of
--         -- the RelayAPI, perhaps to here, and then this, which is really a
--         -- broadcast, could happen here. Right now, we don't actually have info
--         -- about all the connected clients
--         Ocud d -> do
--           -- FIXME: Broadcast should go to everyone, not just providers and
--           -- subscribers
--           generateClientDigests d >>= lift . multicastRev . fmap Frcud
--           unsubDeleted d >>= lift . multicastRev . fmap Frcsd
--         Opd d -> do
--           i' <- maybe (error "no owner for namespace") id .
--             Map.lookup (frpdNamespace d) . nstOwners <$> get
--           send i' $ Frpd d
--         Ope d -> do
--           send i $ Frped d
--           lift $ sendRev $ Right $ ServerDisconnect i
--           handleDisconnect i

-- nonClaim :: TrpDigest -> Bool
-- nonClaim trpd = trpdData trpd == alEmpty && null (trpdContOps trpd)

-- updateOwners
--   :: Monad m => Map Namespace i ->  StateT (NstState i) (NstProtocol m i) ()
-- updateOwners owners = do
--   modify $ \nsts -> nsts {nstOwners = owners}
--   lift $ sendRev $ Left owners

-- throwOutProvider
--   :: (Ord i, Monad m)
--   => i -> Set Namespace -> String -> StateT (NstState i) (NstProtocol m i) ()
-- throwOutProvider i nss msg = do
--   lift $ sendRev $ Right $ ServerData i $ Frped $ FrpErrorDigest $
--     Map.fromSet (const [Text.pack msg]) $
--     Set.map (PathError . (Root :/) . unNamespace) nss
--   lift $ sendRev $ Right $ ServerDisconnect i
--   handleDisconnect i

-- eitherState
--   :: Monad m
--   => (l -> StateT s m b) -> (a -> StateT s m b)
--   -> StateT s (Either l) a -> StateT s m b
-- eitherState onFail onSuccess m = StateT $ \s -> either
--     (\l -> runStateT (onFail l) s)
--     (\(a, s') -> runStateT (onSuccess a) s')
--     (runStateT m s)

-- claimNamespace
--   :: (Eq i, Monad m)
--   => i -> TrpDigest
--   -> (String -> StateT (NstState i) (NstProtocol m i) ())
--   -> StateT (NstState i) (NstProtocol m i) ()
--   -> StateT (NstState i) (NstProtocol m i) ()
-- claimNamespace i d failureAction successAction = either
--     failureAction
--     (\owners' ->
--       maybe (return ()) updateOwners owners' >>
--       successAction)
--     . go . nstOwners =<< get
--   where
--     go owners =
--       let
--         (existing, owners') = Map.insertLookupWithKey
--           (\_ _ _ -> i) (trpdNamespace d) i owners
--       in
--         case existing of
--           Nothing -> if nonClaim d
--             then fail "Empty claim"
--             else return $ Just owners'
--           Just i' -> if (i' == i)
--             then return Nothing
--             else fail "Already owned by someone else guv"

-- relinquishNamespace
--   :: (Eq i, Monad m)
--   => i -> Namespace
--   -> (String -> StateT (NstState i) (NstProtocol m i) ())
--   -> StateT (NstState i) (NstProtocol m i) ()
--   -> StateT (NstState i) (NstProtocol m i) ()
-- relinquishNamespace i ns failureAction successAction = either
--     failureAction
--     (\owners' -> updateOwners owners' >> successAction)
--     . go . nstOwners =<< get
--   where
--     go owners =
--       let
--         (existing, owners') = Map.updateLookupWithKey
--           (\_ _ -> Nothing) ns owners
--       in
--         case existing of
--           Nothing -> fail "You're not the owner"
--           Just i' -> if (i' == i)
--             then return owners'
--             else fail "You're not the owner"

-- unsubscribeFromRelinquishedNs
--   :: Monad m => Set Namespace -> StateT (NstState i) m (Map i FrcSubDigest)
-- unsubscribeFromRelinquishedNs nss = do
--     nsts <- get
--     let partdRegs = partitionRegs <$> nstRegs nsts
--     put $ nsts {nstRegs = snd <$> partdRegs}
--     pure $ mkFrcsd . fst <$> partdRegs
--   where
--     mkFrcsd (ClientRegs goneP goneT goneD) =
--       FrcSubDigest mempty goneP goneT goneD

--     partitionRegs :: ClientRegs -> (ClientRegs, ClientRegs)
--     partitionRegs (ClientRegs p t d) =
--       let
--         part = Mos.partitionKey (`Set.member` nss)
--         (goneP, keepP) = part p
--         (goneT, keepT) = part t
--         (goneD, keepD) = part d
--       in
--         (ClientRegs goneP goneT goneD, ClientRegs keepP keepT keepD)


-- guardNsClientSubDigest
--   :: (Eq i, Monad m)
--   => i -> TrcSubDigest
--   -> (Set Namespace -> String -> StateT (NstState i) (NstProtocol m i) ())
--   -> StateT (NstState i) (NstProtocol m i) ()
--   -> StateT (NstState i) (NstProtocol m i) ()
-- guardNsClientSubDigest i d failureAction successAction = either
--     (uncurry failureAction)
--     (const successAction)
--     . go . nstOwners =<< get
--   where
--     go owners =
--       let
--         ownedAndSubd = Map.keysSet $ Map.filter (== i) $ Map.restrictKeys owners
--           $ trcsdNamespaces d
--       in
--         unless (null ownedAndSubd) $
--           Left (ownedAndSubd, "Acted as client on own namespace")

-- guardNsClientUpdateDigest
--   :: (Eq i, Monad m)
--   => i -> TrcUpdateDigest
--   -> (String -> StateT (NstState i) (NstProtocol m i) ())
--   -> StateT (NstState i) (NstProtocol m i) ()
--   -> StateT (NstState i) (NstProtocol m i) ()
-- guardNsClientUpdateDigest i d failureAction successAction = either
--     failureAction
--     (const successAction)
--     . go . nstOwners =<< get
--   where
--     go owners = maybe
--       (return ())
--       (\i' -> when (i' == i) $ fail "Acted as client on own namespace")
--       $ Map.lookup (trcudNamespace d) owners

-- handleSubDigest
--   :: (Monad m, Ord i)
--   => i -> TrcSubDigest -> StateT (NstState i) m (ClientGetDigest, FrcSubDigest)
-- handleSubDigest i --  (TrcSubDigest ptSubs ptUnsubs tSubs tUnsubs dSubs dUnsubs) =
--   = undefined
--   -- let
--   --   removeUnsubs (ClientRegs p t d) =
--   --     let
--   --       newRegs = ClientRegs
--   --         (Mos.difference p ptUnsubs)
--   --         (Mos.difference t tUnsubs)
--   --         (Mos.difference d dUnsubs)
--   --     in
--   --       if crNull newRegs then Nothing else Just newRegs
--   -- in do
--   --   nsts <- get
--   --   let current = Map.findWithDefault mempty i $ nstRegs nsts
--   --   let cgd = ClientRegs
--   --         (Mos.difference ptSubs $ crPostTypeRegs current)
--   --         (Mos.difference tSubs $ crTypeRegs current)
--   --         (Mos.difference dSubs $ crDataRegs current)
--   --   let frcsd = FrcSubDigest mempty
--   --         (Mos.intersection ptUnsubs $ crPostTypeRegs current)
--   --         (Mos.intersection tUnsubs $ crTypeRegs current)
--   --         (Mos.intersection dUnsubs $ crDataRegs current)
--   --   put $ nsts {nstRegs = Map.update removeUnsubs i $ nstRegs nsts}
--   --   return (cgd, frcsd)

-- handleDisconnect
--   :: (Ord i, Monad m) => i -> StateT (NstState i) (NstProtocol m i) ()
-- handleDisconnect i = do
--     nsts <- get
--     let nsts' = nsts {nstAllClients = Set.delete i $ nstAllClients nsts}
--     let (removed, remaining) = Map.partition (== i) $ nstOwners nsts'
--     unsubscribeFromRelinquishedNs (Map.keysSet removed)
--       >>= lift . multicastRev . fmap Frcsd
--     unless (null removed) $ updateOwners remaining
--     mapM_ send $ Map.keys removed
--   where
--     send ns = lift $ sendFwd (Originator i, PnidTrprd $ TrprDigest ns)

-- registerSubs
--   :: (Ord i, Monad m)
--   => i -> OutboundClientInitialisationDigest -> StateT (NstState i) m ()
-- registerSubs i (FrcUpdateDigest ns postDefs defs _tas dd cops _errs) =
--     modify go
--   where
--     newClientRegs = ClientRegs
--       (Mos.singletonSet ns $ Map.keysSet postDefs)
--       (Mos.singletonSet ns $ Map.keysSet defs)
--       (Mos.singletonSet ns $ Map.keysSet cops <> alKeysSet dd)
--     go nsts = nsts {
--       nstRegs = Map.insertWith (<>) i newClientRegs $ nstRegs nsts}

-- unsubDeleted
--   :: (Monad m, Ord i)
--   => OutboundClientUpdateDigest -> StateT (NstState i) m (Map i FrcSubDigest)
-- unsubDeleted ocud = do
--     nsts <- get
--     let results = fmap updateClientReg $ nstRegs nsts
--     put nsts{ nstRegs = fmap fst results }
--     return $ Map.filter (not . frcsdNull) $ snd <$> results
--   where
--     ns = frcudNamespace ocud
--     allUndefPTys = undefs $ frcudPostDefs ocud
--     allUndefTys = undefs $ frcudDefinitions ocud
--     undefs :: Map (Tagged a Seg) (DefOp a) -> Set (Tagged a Seg)
--     undefs = Map.keysSet . Map.filter isUndef
--     allDeletePaths = Map.keysSet $
--       flattenNestedMaps (:/) $
--       Map.filter isSoAbsent . fmap snd <$> frcudContOps ocud
--     updateClientReg :: ClientRegs -> (ClientRegs, FrcSubDigest)
--     updateClientReg (ClientRegs p t d) =
--       let
--         f s mos = bimap (Mos.singletonSet ns) (\s' -> Mos.replaceSet ns s' mos)
--           $ Set.partition (`Set.member` s) $ Mos.lookup ns mos
--         (nsPUnsubs, p') = f allUndefPTys p
--         (nsTUnsubs, t') = f allUndefTys t
--         (nsDUnsubs, d') = f allDeletePaths d
--       in
--         (ClientRegs p' t' d', FrcSubDigest mempty nsPUnsubs nsTUnsubs nsDUnsubs)

-- multicastRev :: Monad m => Map i FrDigest -> NstProtocol m i ()
-- multicastRev = void . sequence . Map.mapWithKey sendRevWithI
--   where
--     sendRevWithI i digest = sendRev $ Right $ ServerData i digest

-- broadcastRev :: Monad m => FrDigest -> StateT (NstState i) (NstProtocol m i) ()
-- broadcastRev d = get >>= lift . multicastRev . Map.fromSet (const d) . nstAllClients

-- generateClientDigests
--   :: Monad m
--   => OutboundClientUpdateDigest -> StateT (NstState i) m (Map i FrcUpdateDigest)
-- generateClientDigests (FrcUpdateDigest ns ptds tds tas dat cops errs) =
--     Map.filter (not . frcudNull) . fmap filterFrcud . nstRegs <$> get
--   where
--     filterFrcud :: ClientRegs -> FrcUpdateDigest
--     filterFrcud (ClientRegs pt t d) =
--       let
--         nsPs = Mos.lookup ns pt
--         nsTs = Mos.lookup ns t
--         nsDs = Mos.lookup ns d
--       in
--         FrcUpdateDigest ns
--           (Map.restrictKeys ptds nsPs)
--           (Map.restrictKeys tds nsTs)
--           (Map.restrictKeys tas nsDs)
--           (alFilterKey (`Set.member` nsDs) dat)
--           (Map.restrictKeys cops nsDs)
--           (Map.filterWithKey (\dei _ -> case dei of
--             GlobalError -> True
--             PathError p -> p `Set.member` nsDs
--             TimePointError p _ -> p `Set.member` nsDs)
--             errs)

liftedWaitThen ::
  (Monad m, MonadTrans t, Monad (t (Protocol a a' b' b m))) =>
  (a -> t (Protocol a a' b' b m) ()) ->
  (b -> t (Protocol a a' b' b m) ()) ->
  t (Protocol a a' b' b m) ()
liftedWaitThen onFwd onRev = do
  d <- lift wait
  case d of
    Fwd a -> onFwd a
    Rev b -> onRev b

-- zipMapsWithKey
--   :: Ord k
--   => a -> b -> (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
-- zipMapsWithKey defaultA defaultB f = merge
--   (mapMissing $ \k a -> f k a defaultB)
--   (mapMissing $ \k b -> f k defaultA b)
--   (zipWithMatched f)

-- zipMaps
--   :: (Ord k, Monoid a, Monoid b)
--   => (a -> b -> c) -> Map k a -> Map k b -> Map k c
-- zipMaps f = zipMapsWithKey mempty mempty $ const f

-- nestAlByKey
--   :: (Ord k, Ord k0, Ord k1)
--   => (k -> Maybe (k0, k1)) -> AssocList k a
--   -> (AssocList k a, Map k0 (AssocList k1 a))
-- nestAlByKey f = alFoldlWithKey g (alEmpty, mempty)
--   where
--     g (unsplit, nested) k val = case f k of
--       Just (k0, k1) ->
--         ( unsplit
--         , Map.alter (Just . alInsert k1 val . maybe alEmpty id) k0 nested)
--       Nothing -> (alInsert k val unsplit, nested)
