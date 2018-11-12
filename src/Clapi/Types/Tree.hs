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
import Clapi.Types.PNat (SPNat, SomePNat(..), (:<), (%<))
import qualified Clapi.Types.PNat as PNat
import Clapi.Types.SymbolList (Length, SymbolList, SomeSymbolList(..))
import qualified Clapi.Types.SymbolList as SL
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

data EnumVal (sl :: [Symbol]) where
  EnumVal :: (n :< (Length sl) ~ 'True) => SPNat n -> EnumVal sl

enumVal :: MonadFail m => SymbolList sl -> Word32 -> m (EnumVal sl)
enumVal sl w = let pnSl = SL.length sl in case PNat.fromWord32 w of
  SomePNat pnW -> case pnW %< pnSl of
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
