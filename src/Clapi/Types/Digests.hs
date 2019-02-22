{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , KindSignatures
  , GADTs
  , LambdaCase
  , RankNTypes
  , StandaloneDeriving
  , TypeSynonymInstances
  , TemplateHaskell
#-}

module Clapi.Types.Digests where

import Control.Lens (makeLenses)
import Data.Bifunctor (bimap, first)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word32)

import Data.Map.Mol (Mol)
import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Clapi.Types.AssocList (AssocList, alEmpty)
import Clapi.Types.Base (Attributee, Time, TpId, Interpolation)
import Clapi.Types.Definitions
  (SomeDefinition, DefName, PostDefName, Editability, PostDefinition)
import Clapi.Types.Path
  (Seg, Path, pattern (:/), Namespace(..), Placeholder(..))
import Clapi.Types.SequenceOps (SequenceOp(..), isSoAbsent)
import Clapi.Types.Wire (SomeWireValue)


data DataErrorIndex
  = GlobalError
  | NamespaceError Namespace
  | PathError Path
  | TimePointError Path TpId
  -- Placeholder errors need to go somewhere, but potentially not in the data
  -- errs? Perhaps we should remove global errors too and make a bigger sum type
  -- on top?!
  -- | PlaceholderError Placeholder
  deriving (Show, Eq, Ord)

data SubErrorIndex
  = NamespaceSubError Namespace
  | PostTypeSubError Namespace PostDefName
  | TypeSubError Namespace DefName
  | PathSubError Namespace Path
  deriving (Show, Eq, Ord)

class MkSubErrIdx a where
  mkSubErrIdx :: Namespace -> a -> SubErrorIndex

instance MkSubErrIdx PostDefName where
  mkSubErrIdx = PostTypeSubError
instance MkSubErrIdx DefName where
  mkSubErrIdx = TypeSubError
instance MkSubErrIdx Path where
  mkSubErrIdx = PathSubError

data DefOp def = OpDefine {odDef :: def} | OpUndefine deriving (Show, Eq)

isDef :: DefOp a -> Bool
isDef = not . isUndef

isUndef :: DefOp a -> Bool
isUndef OpUndefine = True
isUndef _ = False

data TimeSeriesDataOp =
  OpSet Time [SomeWireValue] Interpolation | OpRemove deriving (Show, Eq)

isRemove :: TimeSeriesDataOp -> Bool
isRemove OpRemove = True
isRemove _ = False

data DataChange
  = ConstChange (Maybe Attributee) [SomeWireValue]
  | TimeChange (Map Word32 (Maybe Attributee, TimeSeriesDataOp))
  deriving (Show, Eq)
type DataDigest = AssocList Path DataChange

data CreateOp
  = OpCreate
  -- FIXME: Nested lists of WireValues is a legacy hangover because our tree
  -- data nodes still contain [WireValue] as a single "value":
  { ocArgs :: [[SomeWireValue]]
  , ocAfter :: Maybe (Either Placeholder Seg)
  } deriving (Show, Eq)
type Creates = Map Path (Map Placeholder (Maybe Attributee, CreateOp))

type RootContOps = Map Namespace (SequenceOp Namespace)
-- FIXME: might this be better as Map (Path, Seg) (blah)? We spend a lot of time
-- coping with the nested map-ness:
type ContOps after = Map Path (Map Seg (Maybe Attributee, SequenceOp after))

data PostOp
  = OpPost {opPath :: Path, opArgs :: Map Seg SomeWireValue} deriving (Show, Eq)

data SubOp = OpSubscribe | OpUnsubscribe deriving (Show, Eq, Enum, Bounded)

isSub :: SubOp -> Bool
isSub OpSubscribe = True
isSub _ = False

data OriginatorRole = Provider | Consumer
data DigestAction = Update | Subscribe | Error | Relinquish | RootUpdate

data TrDigest (r :: OriginatorRole) (a :: DigestAction) where
  Trpd ::
    { trpdNs :: Namespace
    , trpdPostDefs :: Map PostDefName (DefOp PostDefinition)
    , trpdDefs :: Map DefName (DefOp SomeDefinition)
    , trpdData :: DataDigest
    , trpdContOps :: ContOps Seg
    -- FIXME: should errors come in a different digest to data updates? At the
    -- moment we just check a TrpDigest isn't null when processing namespace
    -- claims...
    , trpdErrors :: Mol DataErrorIndex Text
    } -> TrDigest 'Provider 'Update
  Trprd ::
    { trprdNs :: Namespace
    } -> TrDigest 'Provider 'Relinquish

  Trcsd ::
    { trcsdPostTys :: Map (Namespace, PostDefName) SubOp
    , trcsdTys :: Map (Namespace, DefName) SubOp
    , trcsdData :: Map (Namespace, Path) SubOp
    } -> TrDigest 'Consumer 'Subscribe
  Trcud ::
    { trcudNs :: Namespace
    , trcudData :: DataDigest
    , trcudCreates :: Creates
    , trcudContOps :: ContOps (Either Placeholder Seg)
    } -> TrDigest 'Consumer 'Update

deriving instance Show (TrDigest o a)


data FrDigest (r :: OriginatorRole) (a :: DigestAction) where
  Frpd ::
    { frpdNs :: Namespace
    , frpdData :: DataDigest
    , frpdCreates :: Creates
    , frpdContOps :: ContOps (Either Placeholder Seg)
    } -> FrDigest 'Provider 'Update
  Frped ::
    { frpedErrors :: Mol DataErrorIndex Text
    } -> FrDigest 'Provider 'Error

  Frcrd ::
    { frcrdContOps :: RootContOps
    } -> FrDigest 'Consumer 'RootUpdate
  Frcsd ::
    -- FIXME: really this is a Mol:
    { frcsdErrors :: Map SubErrorIndex [Text]
    , frcsdPostTypeUnsubs :: Mos Namespace PostDefName
    , frcsdTypeUnsubs :: Mos Namespace DefName
    , frcsdDataUnsubs :: Mos Namespace Path
    } -> FrDigest 'Consumer 'Subscribe
  Frcud ::
    { frcudNs :: Namespace
    , frcudPostDefs :: Map PostDefName (DefOp PostDefinition)
    , frcudDefs :: Map DefName (DefOp SomeDefinition)
    , frcudTyAssigns :: Map Path (DefName, Editability)
    , frcudData :: DataDigest
    , frcudContOps :: ContOps Seg
    -- FIXME: This could just be for errors that come from providers. Although
    -- we currently send Relay errors here, if we do so we never send any of the
    -- other fields. I.e. we could add an additional Frced type...
    , frcudErrors :: Mol DataErrorIndex Text
    } -> FrDigest 'Consumer 'Update

deriving instance Show (FrDigest o a)

type TrpDigest = TrDigest 'Provider 'Update
type TrprDigest = TrDigest 'Provider 'Relinquish
type TrcSubDigest = TrDigest 'Consumer 'Subscribe
type TrcUpdateDigest = TrDigest 'Consumer 'Update

type FrpDigest = FrDigest 'Provider 'Update
type FrpErrorDigest = FrDigest 'Provider 'Error
type FrcRootDigest = FrDigest 'Consumer 'RootUpdate
type FrcSubDigest = FrDigest 'Consumer 'Subscribe
type FrcUpdateDigest = FrDigest 'Consumer 'Update


data SomeTrDigest where
  SomeTrDigest :: TrDigest o a -> SomeTrDigest

deriving instance Show SomeTrDigest

withTrDigest :: (forall o a. TrDigest o a -> r) -> SomeTrDigest -> r
withTrDigest f (SomeTrDigest d) = f d


data SomeFrDigest where
  SomeFrDigest :: FrDigest o a -> SomeFrDigest

deriving instance Show SomeFrDigest

withFrDigest :: (forall o a. FrDigest o a -> r) -> SomeFrDigest -> r
withFrDigest f (SomeFrDigest d) = f d

trDigestNull :: TrDigest o a -> Bool
trDigestNull = \case
  Trpd _ pds ds dat cops errs ->
    null pds && null ds && null dat && null cops && null errs
  Trprd _ -> False
  Trcsd ptys tys dat -> null ptys && null tys && null dat
  Trcud _ dat crs cops -> null dat && null crs && null cops

frDigestNull :: FrDigest o a -> Bool
frDigestNull = \case
  Frpd _ dat crs cops -> null dat && null crs && null cops
  Frped errs -> null errs
  Frcrd cops -> null cops
  Frcsd errs ptys tys dat -> null errs && null ptys && null tys && null dat
  Frcud _ pds ds tys dat cops errs ->
    null pds && null ds && null tys && null dat && null cops && null errs

trpdEmpty :: Namespace -> TrpDigest
trpdEmpty ns = Trpd ns mempty mempty alEmpty mempty mempty

trpdRemovedPaths :: TrpDigest -> [Path]
trpdRemovedPaths trpd =
    Map.foldlWithKey f [] (trpdContOps trpd)
  where
    f acc p segMap = acc ++
      (fmap (p :/) $ Map.keys $ Map.filter isSoAbsent $ fmap snd segMap)

frpdEmpty :: Namespace -> FrpDigest
frpdEmpty ns = Frpd ns mempty mempty mempty

trcsdEmpty :: TrcSubDigest
trcsdEmpty = mempty

instance Semigroup TrcSubDigest where
  Trcsd p1 t1 d1 <> Trcsd p2 t2 d2 = Trcsd
      (p2 <> p1) (t2 <> t1) (d2 <> d1)

instance Monoid TrcSubDigest where
  mempty = Trcsd mempty mempty mempty

trcsdNamespaces :: TrcSubDigest -> Set Namespace
trcsdNamespaces (Trcsd p t d) =
  let f = Set.mapMonotonic fst . Map.keysSet in mconcat [f p, f t, f d]

data ClientRegs
  = ClientRegs
  { _crPostTypeRegs :: Mos Namespace PostDefName
  , _crTypeRegs :: Mos Namespace DefName
  , _crDataRegs :: Mos Namespace Path
  } deriving (Show)

makeLenses 'ClientRegs

instance Semigroup ClientRegs where
  (ClientRegs pt1 t1 d1) <> (ClientRegs pt2 t2 d2) =
    ClientRegs (pt1 <> pt2) (t1 <> t2) (d1 <> d2)

instance Monoid ClientRegs where
  mempty = ClientRegs mempty mempty mempty

crNull :: ClientRegs -> Bool
crNull (ClientRegs p t d) = null p && null t && null d

crDifference :: ClientRegs -> ClientRegs -> ClientRegs
crDifference (ClientRegs p1 t1 d1) (ClientRegs p2 t2 d2) = ClientRegs
  (Mos.difference p1 p2)
  (Mos.difference t1 t2)
  (Mos.difference d1 d2)

crIntersection :: ClientRegs -> ClientRegs -> ClientRegs
crIntersection (ClientRegs p1 t1 d1) (ClientRegs p2 t2 d2) = ClientRegs
  (Mos.intersection p1 p2)
  (Mos.intersection t1 t2)
  (Mos.intersection d1 d2)

crDeleteLookupNs :: Namespace -> ClientRegs -> (ClientRegs, ClientRegs)
crDeleteLookupNs ns (ClientRegs p t d) =
  let
    f :: Ord a => Mos Namespace a -> (Mos Namespace a, Mos Namespace a)
    f = first (Mos.singletonSet ns) . Mos.deleteLookupSet ns
    (loseP, keepP) = f p
    (loseT, keepT) = f t
    (loseD, keepD) = f d
  in
    (ClientRegs loseP loseT loseD, ClientRegs keepP keepT keepD)

trcsdClientRegs :: TrcSubDigest -> (ClientRegs, ClientRegs)
trcsdClientRegs (Trcsd p t d) =
  let
    f :: Ord x => Map (Namespace, x) SubOp -> (Mos Namespace x, Mos Namespace x)
    f = bimap mosify mosify . Map.partition isSub
    mosify :: Ord x => Map (Namespace, x) a -> Mos Namespace x
    mosify = Mos.fromList . Map.keys
    (pSub, pUnsub) = f p
    (tSub, tUnsub) = f t
    (dSub, dUnsub) = f d
  in
    (ClientRegs pSub tSub dSub, ClientRegs pUnsub tUnsub dUnsub)

trcudEmpty :: Namespace -> TrcUpdateDigest
trcudEmpty ns = Trcud ns mempty mempty mempty

instance Semigroup FrpDigest where
  Frpd _ dat1 cr1 cops1 <> Frpd ns dat2 cr2 cops2 =
    Frpd ns (dat1 <> dat2) (cr2 <> cr1) (cops2 <> cops1)

instance Semigroup FrpErrorDigest where
  Frped e1 <> Frped e2 = Frped $ e1 <> e2

instance Monoid FrpErrorDigest where
  mempty = Frped mempty

instance Semigroup FrcRootDigest where
  Frcrd cops1 <> Frcrd cops2 = Frcrd $ cops2 <> cops1

instance Monoid FrcRootDigest where
  mempty = Frcrd mempty

instance Semigroup FrcSubDigest where
  Frcsd e1 pt1 t1 d1 <> Frcsd e2 pt2 t2 d2 =
    Frcsd (e1 <> e2) (pt1 <> pt2) (t1 <> t2) (d1 <> d2)

instance Monoid FrcSubDigest where
  mempty = Frcsd mempty mempty mempty mempty

instance Semigroup FrcUpdateDigest where
  Frcud _ pds1 ds1 tys1 dat1 cops1 errs1
    <> Frcud ns pds2 ds2 tys2 dat2 cops2 errs2 =
      Frcud ns (pds2 <> pds1) (ds2 <> ds1) (tys2 <> tys1) (dat1 <> dat2)
      (cops2 <> cops1) (errs1 <> errs2)

frcsdEmpty :: FrcSubDigest
frcsdEmpty = mempty

frcsdFromClientRegs :: ClientRegs -> FrcSubDigest
frcsdFromClientRegs (ClientRegs p t d) = Frcsd mempty p t d

frcudEmpty :: Namespace -> FrcUpdateDigest
frcudEmpty ns = Frcud ns mempty mempty mempty alEmpty mempty mempty
