{-# LANGUAGE
    FlexibleInstances
  , LambdaCase
  , TypeSynonymInstances
#-}

module Clapi.Types.Digests where

import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word32)

import Data.Map.Mol (Mol)
import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Clapi.Types.AssocList (AssocList, alNull, alEmpty)
import Clapi.Types.Base (Attributee, Time, Interpolation)
import Clapi.Types.Definitions
  (Definition, SomeDefinition, Editable, PostDefinition, DefName, PostDefName)
import Clapi.Types.Path
  (Seg, Path, pattern (:/), Namespace(..), Placeholder(..))
import Clapi.Types.SequenceOps (SequenceOp(..), isSoAbsent)
import Clapi.Types.Wire (SomeWireValue)


type TpId = Word32

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

isUndef :: DefOp a -> Bool
isUndef OpUndefine = True
isUndef _ = False

isDef :: DefOp a -> Bool
isDef = not . isUndef

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

data TrpDigest = TrpDigest
  { trpdNamespace :: Namespace
  , trpdPostDefs :: Map PostDefName (DefOp PostDefinition)
  , trpdDefinitions :: Map DefName (DefOp SomeDefinition)
  , trpdData :: DataDigest
  , trpdContOps :: ContOps Seg
  -- FIXME: should errors come in a different digest to data updates? At the
  -- moment we just check a TrpDigest isn't null when processing namespace
  -- claims...
  , trpdErrors :: Mol DataErrorIndex Text
  } deriving (Show)

trpdEmpty :: Namespace -> TrpDigest
trpdEmpty ns = TrpDigest ns mempty mempty alEmpty mempty mempty

trpdRemovedPaths :: TrpDigest -> [Path]
trpdRemovedPaths trpd =
    Map.foldlWithKey f [] (trpdContOps trpd)
  where
    f acc p segMap = acc ++
      (fmap (p :/) $ Map.keys $ Map.filter isSoAbsent $ fmap snd segMap)

trpdNull :: TrpDigest -> Bool
trpdNull (TrpDigest _ns postDefs defs dd cops errs) =
  null postDefs && null defs && alNull dd && null cops && null errs

data FrpDigest = FrpDigest
  { frpdNamespace :: Namespace
  , frpdData :: DataDigest
  , frpdCreates :: Creates
  , frpdContOps :: ContOps (Either Placeholder Seg)
  } deriving (Show, Eq)

frpdEmpty :: Namespace -> FrpDigest
frpdEmpty ns = FrpDigest ns mempty mempty mempty

frpdNull :: FrpDigest -> Bool
frpdNull (FrpDigest _ dd creates cops) = null dd && null creates && null cops

newtype FrpErrorDigest = FrpErrorDigest
  { frpedErrors :: Mol DataErrorIndex Text
  } deriving (Show, Eq)

frpedNull :: FrpErrorDigest -> Bool
frpedNull = null . frpedErrors

data SubOp = OpSubscribe | OpUnsubscribe deriving (Show, Eq, Enum, Bounded)

isSub :: SubOp -> Bool
isSub OpSubscribe = True
isSub _ = False

data TrcSubDigest = TrcSubDigest
  { trcsdPostTypes :: Map (Namespace, PostDefName) SubOp
  , trcsdTypes :: Map (Namespace, DefName) SubOp
  , trcsdData :: Map (Namespace, Path) SubOp
  } deriving (Show, Eq)

trcsdEmpty :: TrcSubDigest
trcsdEmpty = mempty

instance Semigroup TrcSubDigest where
  TrcSubDigest p1 t1 d1 <> TrcSubDigest p2 t2 d2 = TrcSubDigest
      (p2 <> p1) (t2 <> t1) (d2 <> d1)

instance Monoid TrcSubDigest where
  mempty = TrcSubDigest mempty mempty mempty

trcsdNamespaces :: TrcSubDigest -> Set Namespace
trcsdNamespaces (TrcSubDigest p t d) =
  let f = Set.mapMonotonic fst . Map.keysSet in mconcat [f p, f t, f d]

data ClientRegs
  = ClientRegs
  { crPostTypeRegs :: Mos Namespace PostDefName
  , crTypeRegs :: Mos Namespace DefName
  , crDataRegs :: Mos Namespace Path
  } deriving (Show)

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

trcsdClientRegs :: TrcSubDigest -> (ClientRegs, ClientRegs)
trcsdClientRegs (TrcSubDigest p t d) =
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

data TrcUpdateDigest = TrcUpdateDigest
  { trcudNamespace :: Namespace
  , trcudData :: DataDigest
  , trcudCreates :: Creates
  , trcudContOps :: ContOps (Either Placeholder Seg)
  } deriving (Show, Eq)

trcudEmpty :: Namespace -> TrcUpdateDigest
trcudEmpty ns = TrcUpdateDigest ns mempty mempty mempty

newtype FrcRootDigest = FrcRootDigest
  { frcrdContOps :: RootContOps
  } deriving (Show, Eq)

frcrdNull :: FrcRootDigest -> Bool
frcrdNull = null . frcrdContOps

data FrcSubDigest = FrcSubDigest
  -- FIXME: really this is a Mol:
  { frcsdErrors :: Map SubErrorIndex [Text]
  , frcsdPostTypeUnsubs :: Mos Namespace PostDefName
  , frcsdTypeUnsubs :: Mos Namespace DefName
  , frcsdDataUnsubs :: Mos Namespace Path
  } deriving (Show, Eq)

frcsdNull :: FrcSubDigest -> Bool
frcsdNull (FrcSubDigest errs ptUnsubs tUnsubs dUnsubs) =
  null errs && null ptUnsubs && null tUnsubs && null dUnsubs

instance Semigroup FrcSubDigest where
  (FrcSubDigest e1 pt1 t1 d1) <> (FrcSubDigest e2 pt2 t2 d2) =
    FrcSubDigest (e1 <> e2) (pt1 <> pt2) (t1 <> t2) (d1 <> d2)

instance Monoid FrcSubDigest where
  mempty = FrcSubDigest mempty mempty mempty mempty

frcsdEmpty :: FrcSubDigest
frcsdEmpty = mempty

frcsdFromClientRegs :: ClientRegs -> FrcSubDigest
frcsdFromClientRegs (ClientRegs p t d) = FrcSubDigest mempty p t d

data FrcUpdateDigest = FrcUpdateDigest
  { frcudNamespace :: Namespace
  , frcudPostDefs :: Map PostDefName (DefOp PostDefinition)
  , frcudDefinitions :: Map DefName (DefOp SomeDefinition)
  , frcudTypeAssignments :: Map Path (DefName, Editable)
  , frcudData :: DataDigest
  , frcudContOps :: ContOps Seg
  , frcudErrors :: Mol DataErrorIndex Text
  } deriving (Show)

frcudEmpty :: Namespace -> FrcUpdateDigest
frcudEmpty ns = FrcUpdateDigest ns mempty mempty mempty alEmpty mempty mempty

frcudNull :: FrcUpdateDigest -> Bool
frcudNull (FrcUpdateDigest _ postDefs defs tas dd cops errs) =
  null postDefs && null defs && null tas && null dd && null cops && null errs

newtype TrprDigest
  = TrprDigest {trprdNamespace :: Namespace}
  deriving (Show, Eq)

data TrDigest
  = Trpd TrpDigest
  | Trprd TrprDigest
  | Trcsd TrcSubDigest
  | Trcud TrcUpdateDigest
  deriving (Show)

data FrDigest
  = Frpd FrpDigest
  | Frped FrpErrorDigest
  | Frcrd FrcRootDigest
  | Frcsd FrcSubDigest
  | Frcud FrcUpdateDigest
  deriving (Show)

frNull :: FrDigest -> Bool
frNull = \case
  Frpd d -> frpdNull d
  Frped d -> frpedNull d
  Frcrd d -> frcrdNull d
  Frcsd d -> frcsdNull d
  Frcud d -> frcudNull d

-- | "Split" because kinda like :: Map k1 a -> Map k2 (Map k3 a)
splitMap :: (Ord a, Ord b) => [(a, (b, c))] -> Map a (Map b c)
splitMap = foldl mush mempty
  where
    mush m (a, bc) = Map.alter (mush' bc) a m
    mush' (b, c) = Just . Map.insert b c . maybe mempty id

-- The following are slightly different (and more internal to the relay), they
-- are not neccessarily intended for a single recipient

type OutboundClientUpdateDigest = FrcUpdateDigest
type OutboundClientInitialisationDigest = OutboundClientUpdateDigest
type OutboundClientSubErrsDigest = Map SubErrorIndex [Text]
type OutboundProviderDigest = FrpDigest

ocsedNull :: OutboundClientSubErrsDigest -> Bool
ocsedNull = null

data OutboundDigest
  = Ocrid FrcRootDigest  -- "Outbound client root initialisation digest"
  | Ocid OutboundClientInitialisationDigest
  | Ocsed OutboundClientSubErrsDigest
  | Ocrd FrcRootDigest
  | Ocud OutboundClientUpdateDigest
  | Opd OutboundProviderDigest
  | Ope FrpErrorDigest
  deriving (Show)
