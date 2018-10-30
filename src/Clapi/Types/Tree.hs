{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    FunctionalDependencies
  , GADTs
  , MultiParamTypeClasses
  , ScopedTypeVariables
#-}

module Clapi.Types.Tree
  ( Bounds, bounds, unbounded, boundsMin, boundsMax
  , TypeEnumOf(..)
  , TreeType(..), TreeTypeName(..), ttEnum

  , NewTreeType(..), SomeTreeType(..), EnumWord32(..)
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Int
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word

import Clapi.Types.Base (Time)
import Clapi.Types.Path (Path, Seg, mkSeg)
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

-- | A Word32 that's being used to represent the constructor of the phantom Enum
--   type `a`
newtype EnumWord32 a where
  EnumWord32 :: Word32 -> EnumWord32 a

type SimpleTreeType a = NewTreeType a a

data NewTreeType a b where
  NttTime :: SimpleTreeType Time
  NttEnum :: (Bounded b, Enum b) => NewTreeType Word32 b
  NttWord32 :: Bounds Word32 -> SimpleTreeType Word32
  NttString :: Text -> SimpleTreeType Text
  NttRef :: Seg -> NewTreeType Text Path
  NttList :: NewTreeType a b -> NewTreeType [a] [b]
  NttPair
    :: NewTreeType x1 x2 -> NewTreeType y1 y2 -> NewTreeType (x1, y1) (x2, y2)

data SomeTreeType where
  SomeTreeType :: NewTreeType a b -> SomeTreeType


class (Bounded b, Enum b) => TypeEnumOf a b | a -> b where
  typeEnumOf :: a -> b

data TreeType
  = TtTime
  | TtEnum [Seg]
  | TtWord32 (Bounds Word32)
  | TtWord64 (Bounds Word64)
  | TtInt32 (Bounds Int32)
  | TtInt64 (Bounds Int64)
  | TtFloat (Bounds Float)
  | TtDouble (Bounds Double)
  | TtString Text
  -- FIXME: kinda want this to be TtRef (Tagged Definition TypeName) but that
  -- creates an import loop:
  | TtRef Seg
  | TtList TreeType
  | TtSet TreeType
  | TtOrdSet TreeType
  | TtMaybe TreeType
  | TtPair TreeType TreeType
  deriving (Show, Eq, Ord)

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

ttEnum :: forall a. (Enum a, Bounded a, Show a) => Proxy a -> TreeType
ttEnum _ = TtEnum $
  fmap (fromJust . mkSeg . Text.pack . uncamel . show) [minBound :: a..]
