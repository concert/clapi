{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , KindSignatures
  , LambdaCase
  , MultiParamTypeClasses
  , PartialTypeSignatures
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Clapi.Types.Tree
  -- ( Bounds, bounds, unbounded, boundsMin, boundsMax
  -- , TypeEnumOf(..)
  -- , TreeType(..), TreeTypeName(..)

  -- , SomeTreeType(..)
  -- , ttTime, ttWord32, ttFloat, ttString, ttRef, ttList
  -- ) where
  where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Constraint (Dict(..))
import Data.Finite (Finite, packFinite)
import Data.Int
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Type.Equality ((:~:)(..))
import Data.Word

-- For `Symbol` and `(+)`. I can't seem to import `(+)` explicityly :-/
import GHC.TypeLits

import Clapi.Types.Base (Time)
import Clapi.Types.Path (Path, Seg, mkSeg, unSeg)
import Clapi.Types.UniqList (UniqList)
import Clapi.Types.Wire (WireType(..), SomeWireType(..))
import Clapi.Util (uncamel)


data Bounds a
  = Bounds (Maybe a) (Maybe a)
  deriving (Show, Eq, Ord)

boundsMin, boundsMax :: Bounds a -> Maybe a
boundsMin (Bounds m _) = m
boundsMax (Bounds _ m) = m

bounds :: (MonadFail m, Ord a) => Maybe a -> Maybe a -> m (Bounds a)
bounds m0@(Just bMin) m1@(Just bMax)
    | bMin <= bMax = return $ Bounds m0 m1
    | otherwise = fail "minBound > maxBound"
bounds m0 m1 = return $ Bounds m0 m1

unbounded :: Bounds a
unbounded = Bounds Nothing Nothing

data PNat = Zero | Succ PNat deriving (Show)

data SPNat (a :: PNat) where
  SPZero :: SPNat 'Zero
  SPSucc :: SPNat a -> SPNat ('Succ a)

instance Show (SPNat a) where
  show = show . unPNat

data SomePNat where
  SomePNat :: SPNat (a :: PNat) -> SomePNat
deriving instance Show (SomePNat)

type family Natty (n :: Nat) :: PNat where
  Natty 0 = 'Zero
  Natty n = 'Succ (Natty (n - 1))

type family UnNatty (n :: PNat) :: Nat where
  UnNatty 'Zero = 0
  UnNatty ('Succ n) = 1 + UnNatty n

pNat' :: forall r. (forall x. SPNat x -> r) -> Word32 -> r
pNat' f = go SPZero
  where
    go :: SPNat x -> Word32 -> r
    go pn 0 = f pn
    go pn n = go (SPSucc pn) (n - 1)

pNat :: Word32 -> SomePNat
pNat = pNat' SomePNat

unPNat :: SPNat a -> Word32
unPNat = \case
  SPZero -> 0
  SPSucc pn -> 1 + unPNat pn

type family (m :: PNat) :<? (n :: PNat) :: Bool where
  n :<? 'Zero = 'False
  'Zero :<? 'Succ n = 'True
  'Succ m :<? 'Succ n = m :<? n

type m :< n = (m :<? n ~ 'True)

prooveLessThan :: SPNat m -> SPNat n -> Maybe (m :<? n :~: 'True)
prooveLessThan m SPZero = Nothing
prooveLessThan SPZero (SPSucc n) = Just Refl
prooveLessThan (SPSucc m) (SPSucc n) = prooveLessThan m n

segSym :: Seg -> SomeSymbol
segSym = someSymbolVal . Text.unpack . unSeg

data SymbolList (a :: [Symbol]) where
  SlEmpty :: SymbolList '[]
  SlCons :: KnownSymbol s => Proxy s -> SymbolList sl -> SymbolList ('(:) s sl)
deriving instance Show (SymbolList a)

slSingleton :: KnownSymbol s => Proxy s -> SymbolList '[s]
slSingleton p = SlCons p SlEmpty

data SomeSymbolList where
  SomeSymbolList :: SymbolList sl -> SomeSymbolList
deriving instance Show (SomeSymbolList)

type family SlConcat (a1 :: [k]) (a2 :: [k]) :: [k] where
  SlConcat '[] a2s = a2s
  SlConcat ('(:) a1 a1s) a2s = a1 : SlConcat a1s a2s

slConcat :: SymbolList sl1 -> SymbolList sl2 -> SymbolList (SlConcat sl1 sl2)
slConcat SlEmpty a2s = a2s
slConcat (SlCons a1 a1s) a2s = SlCons a1 (slConcat a1s a2s)

slMconcat :: [SomeSymbolList] -> SomeSymbolList
slMconcat = go SlEmpty
  where
    go :: SymbolList sl -> [SomeSymbolList] -> SomeSymbolList
    go acc [] = SomeSymbolList acc
    go acc (ssl : ssls) = case ssl of
      (SomeSymbolList sl) -> go (slConcat acc sl) ssls

withKnownSymbol :: (forall s. KnownSymbol s => Proxy s -> r) -> String -> r
withKnownSymbol f s = case someSymbolVal s of
  (SomeSymbol p) -> f p

symbolList :: [String] -> SomeSymbolList
symbolList = slMconcat . fmap (withKnownSymbol $ SomeSymbolList . slSingleton)

unSymbolList :: SymbolList sl -> [String]
unSymbolList = \case
  SlEmpty -> []
  SlCons p sl -> symbolVal p : unSymbolList sl

unSymbolList_ :: SomeSymbolList -> [String]
unSymbolList_ (SomeSymbolList sl) = unSymbolList sl

type family SlLength (a :: [k]) :: PNat where
  SlLength '[] = 'Zero
  SlLength ('(:) a as) = 'Succ (SlLength as)

slLength :: SymbolList sl -> SPNat (SlLength sl)
slLength = \case
  SlEmpty -> SPZero
  SlCons _ sl -> SPSucc $ slLength sl


data EnumVal (sl :: [Symbol]) where
  EnumVal :: (n :< (SlLength sl)) => SPNat n -> EnumVal sl

enumVal :: MonadFail m => SymbolList sl -> Word32 -> m (EnumVal sl)
enumVal sl w = let pnSl = slLength sl in case pNat w of
  SomePNat pnW -> case prooveLessThan pnW pnSl of
    Nothing -> fail "darf"
    Just Refl -> return $ EnumVal pnW

-- | A value-level witness for any type that can be held in the CLAPI tree
data SingTreeType a where
  SttTime :: SingTreeType Time
  SttEnum :: SymbolList sl -> SingTreeType (EnumVal sl)
  SttWord32 :: SingTreeType Word32
  SttRef :: SingTreeType Path
  SttList :: SingTreeType a -> SingTreeType [a]
  SttSet :: SingTreeType a -> SingTreeType (Set a)
  SttPair :: SingTreeType a -> SingTreeType b -> SingTreeType (a, b)

-- | What type of value do we need to read off the wire to get a value we can
--   put in the tree?
sttWireType :: SingTreeType a -> SomeWireType
sttWireType = \case
    SttTime -> SomeWireType WtTime
    SttWord32 -> SomeWireType WtWord32
    SttRef -> SomeWireType WtString
    SttList stt -> wrap (SomeWireType . WtList) stt
    SttSet stt -> wrap (SomeWireType . WtList) stt
    SttPair stt1 stt2 -> case (sttWireType stt1, sttWireType stt2) of
      (SomeWireType wt1, SomeWireType wt2) -> SomeWireType $ WtPair wt1 wt2
  where
    wrap :: (forall x. WireType x -> SomeWireType) -> SingTreeType a -> SomeWireType
    wrap con stt = case sttWireType stt of (SomeWireType wt) -> con wt

data TreeType where
  TtTime :: TreeType
  TtEnum :: [String] -> TreeType
  TtWord32 :: Bounds Word32 -> TreeType
  TtWord64 :: Bounds Word64 -> TreeType
  TtInt32 :: Bounds Int32 -> TreeType
  TtInt64 :: Bounds Int64 -> TreeType
  TtFloat :: Bounds Float -> TreeType
  TtDouble :: Bounds Double -> TreeType
  TtString :: Text -> TreeType
  -- FIXME: kinda want this to be TtRef (Tagged Definition TypeName) but that
  -- creates an import loop:
  TtRef :: Seg -> TreeType
  TtList :: TreeType -> TreeType
  TtSet :: TreeType -> TreeType
  TtOrdSet :: TreeType -> TreeType
  TtMaybe :: TreeType -> TreeType
  TtPair :: TreeType -> TreeType -> TreeType

deriving instance Show TreeType

ttWireType :: TreeType -> SomeWireType
ttWireType = \case
  TtTime -> SomeWireType WtTime
  TtEnum _ -> SomeWireType  WtWord32
  TtWord32 _ -> SomeWireType WtWord32
  TtRef _ -> SomeWireType WtString
  TtList tt -> case ttWireType tt of
    (SomeWireType wt) -> SomeWireType $ WtList wt
  TtSet tt -> case ttWireType tt of
    (SomeWireType wt) -> SomeWireType $ WtList wt
  TtOrdSet tt -> case ttWireType tt of
    (SomeWireType wt) -> SomeWireType $ WtList wt
  TtMaybe tt -> case ttWireType tt of
    (SomeWireType wt) -> SomeWireType $ WtMaybe wt
  TtPair tt1 tt2 -> case (ttWireType tt1, ttWireType tt2) of
    (SomeWireType wt1, SomeWireType wt2) -> SomeWireType $ WtPair wt1 wt2

ttEnum :: [Seg] -> TreeType
ttEnum = TtEnum . fmap (Text.unpack . unSeg)

ttEnum' :: forall a. (Bounded a, Enum a, Show a) => Proxy a -> TreeType
ttEnum' _ = TtEnum $ uncamel . show <$> [minBound @a ..]


class (Bounded b, Enum b) => TypeEnumOf a b | a -> b where
  typeEnumOf :: a -> b

data TreeTypeName
  = TtnTime | TtnEnum
  | TtnWord32 | TtnWord64 | TtnInt32 | TtnInt64 | TtnFloat | TtnDouble
  | TtnString | TtnRef
  | TtnList | TtnSet | TtnOrdSet
  | TtnMaybe
  | TtnPair
  deriving (Show, Eq, Ord, Enum, Bounded)

instance TypeEnumOf TreeType TreeTypeName where
  typeEnumOf tt = case tt of
    TtTime -> TtnTime
    TtEnum _ -> TtnEnum
    TtWord32 _ -> TtnWord32
    TtWord64 _ -> TtnWord64
    TtInt32 _ -> TtnInt32
    TtInt64 _ -> TtnInt64
    TtFloat _ -> TtnFloat
    TtDouble _ -> TtnDouble
    TtString _ -> TtnString
    TtRef _ -> TtnRef
    TtList _ -> TtnList
    TtSet _ -> TtnSet
    TtOrdSet _ -> TtnOrdSet
    TtMaybe _ -> TtnMaybe
    TtPair _ _ -> TtnPair
