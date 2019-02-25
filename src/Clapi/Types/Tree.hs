{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , RankNTypes
  , StandaloneDeriving
  , TemplateHaskell
  , TypeFamilies
#-}

module Clapi.Types.Tree
  ( Bounds, bounds, unbounded, boundsMin, boundsMax
  , TreeType(..), SomeTreeType(..), withTreeType
  , ttTime, ttEnum, ttEnum'
  , ttWord32, ttWord64, ttInt32, ttInt64, ttFloat, ttDouble
  , ttString, ttRef
  , ttList, ttSet, ttOrdSet, ttMaybe
  , ttPair

  , getTtShow, getTtOrd

  , WireTypeOf, wireTypeOf
  , TreeTypeName(..)
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Int
import Data.Set (Set)
import Data.Text (Text)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import Data.Word
import Language.Haskell.TH (Type(ConT))

import Clapi.Internal.TreeType (Bounds(..), TreeType(..))
import Clapi.Types.Base (Time, TypeEnumOf(..))
import Clapi.Types.EnumVal (EnumVal)
import Clapi.Types.Path (Name, Path)
import Clapi.Types.SymbolList (withSymbolListFromStrings)
import Clapi.Types.TreeTH (mkGetTtConstraint)
import Clapi.Types.UniqList (UniqList)
import Clapi.Types.Wire (WireType(..))
import Clapi.Util (uncamel, liftRefl, pairRefl)

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


deriving instance Show (TreeType a)
deriving instance Eq (TreeType a)

instance TestEquality TreeType where
  testEquality TtTime TtTime = Just Refl
  testEquality (TtEnum sl1) (TtEnum sl2) = liftRefl <$> testEquality sl1 sl2
  testEquality (TtWord32 _) (TtWord32 _) = Just Refl
  testEquality (TtWord64 _) (TtWord64 _) = Just Refl
  testEquality (TtInt32 _) (TtInt32 _) = Just Refl
  testEquality (TtInt64 _) (TtInt64 _) = Just Refl
  testEquality (TtFloat _) (TtFloat _) = Just Refl
  testEquality (TtDouble _) (TtDouble _) = Just Refl
  testEquality (TtString _) (TtString _) = Just Refl
  testEquality (TtRef _) (TtRef _) = Just Refl
  testEquality (TtList tt1) (TtList tt2) = liftRefl <$> testEquality tt1 tt2
  testEquality (TtSet tt1) (TtSet tt2) = liftRefl <$> testEquality tt1 tt2
  testEquality (TtOrdSet tt1) (TtOrdSet tt2) = liftRefl <$> testEquality tt1 tt2
  testEquality (TtMaybe tt1) (TtMaybe tt2) = liftRefl <$> testEquality tt1 tt2
  testEquality (TtPair tt1x tt1y) (TtPair tt2x tt2y) =
    pairRefl <$> testEquality tt1x tt2x <*> testEquality tt1y tt2y
  testEquality _ _ = Nothing


data SomeTreeType where
  SomeTreeType :: TreeType a -> SomeTreeType
deriving instance Show SomeTreeType

instance Eq SomeTreeType where
  SomeTreeType tt1 == SomeTreeType tt2 = case testEquality tt1 tt2 of
    Just Refl -> tt1 == tt2
    Nothing -> False

withTreeType :: (forall a. TreeType a -> r) -> SomeTreeType -> r
withTreeType f (SomeTreeType tt) = f tt


ttTime :: SomeTreeType
ttTime = SomeTreeType TtTime

ttEnum :: [String] -> SomeTreeType
ttEnum = withSymbolListFromStrings $ SomeTreeType . TtEnum

ttEnum'
  :: forall proxy e. (Bounded e, Enum e, Show e) => proxy e -> SomeTreeType
ttEnum' _ = ttEnum $ uncamel . show <$> [minBound @e ..]

ttWord32 :: Bounds Word32 -> SomeTreeType
ttWord32 = SomeTreeType . TtWord32

ttWord64 :: Bounds Word64 -> SomeTreeType
ttWord64 = SomeTreeType . TtWord64

ttInt32 :: Bounds Int32 -> SomeTreeType
ttInt32 = SomeTreeType . TtInt32

ttInt64 :: Bounds Int64 -> SomeTreeType
ttInt64 = SomeTreeType . TtInt64

ttFloat :: Bounds Float -> SomeTreeType
ttFloat = SomeTreeType . TtFloat

ttDouble :: Bounds Double -> SomeTreeType
ttDouble = SomeTreeType . TtDouble

ttString :: Text -> SomeTreeType
ttString = SomeTreeType . TtString

ttRef :: Name -> SomeTreeType
ttRef = SomeTreeType . TtRef

ttList :: SomeTreeType -> SomeTreeType
ttList (SomeTreeType tt) = SomeTreeType $ TtList tt

ttSet :: SomeTreeType -> SomeTreeType
ttSet (SomeTreeType tt) = SomeTreeType $ TtSet tt

ttOrdSet :: SomeTreeType -> SomeTreeType
ttOrdSet (SomeTreeType tt) = SomeTreeType $ TtOrdSet tt

ttMaybe :: SomeTreeType -> SomeTreeType
ttMaybe (SomeTreeType tt) = SomeTreeType $ TtMaybe tt

ttPair :: SomeTreeType -> SomeTreeType -> SomeTreeType
ttPair (SomeTreeType tt1) (SomeTreeType tt2) = SomeTreeType $ TtPair tt1 tt2


mkGetTtConstraint "getTtShow" $ ConT ''Show
mkGetTtConstraint "getTtOrd" $ ConT ''Ord


-- | What type of value do we need to read off the wire to get a value we can
--   put in the tree?
type family WireTypeOf a where
  WireTypeOf Time = Time
  WireTypeOf (EnumVal ss) = Word32
  WireTypeOf Word32 = Word32
  WireTypeOf Word64 = Word64
  WireTypeOf Int32 = Int32
  WireTypeOf Int64 = Int64
  WireTypeOf Float = Float
  WireTypeOf Double = Double
  WireTypeOf Text = Text
  WireTypeOf Path = Text
  WireTypeOf [a] = [WireTypeOf a]
  WireTypeOf (Set a) = [WireTypeOf a]
  WireTypeOf (UniqList a) = [WireTypeOf a]
  WireTypeOf (Maybe a) = Maybe (WireTypeOf a)
  WireTypeOf (a, b) = (WireTypeOf a, WireTypeOf b)

wireTypeOf :: TreeType a -> WireType (WireTypeOf a)
wireTypeOf = \case
  TtTime -> WtTime
  TtEnum _ -> WtWord32
  TtWord32 _ -> WtWord32
  TtWord64 _ -> WtWord64
  TtInt32 _ -> WtInt32
  TtInt64 _ -> WtInt64
  TtFloat _ -> WtFloat
  TtDouble _ -> WtDouble
  TtString _ -> WtString
  TtRef _ -> WtString
  TtList tt -> WtList $ wireTypeOf tt
  TtSet tt -> WtList $ wireTypeOf tt
  TtOrdSet tt -> WtList $ wireTypeOf tt
  TtMaybe tt -> WtMaybe $ wireTypeOf tt
  TtPair tt1 tt2 -> WtPair (wireTypeOf tt1) (wireTypeOf tt2)


data TreeTypeName
  = TtnTime | TtnEnum
  | TtnWord32 | TtnWord64 | TtnInt32 | TtnInt64 | TtnFloat | TtnDouble
  | TtnString | TtnRef
  | TtnList | TtnSet | TtnOrdSet
  | TtnMaybe
  | TtnPair
  deriving (Show, Eq, Ord, Enum, Bounded)

instance TypeEnumOf (TreeType a) TreeTypeName where
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
