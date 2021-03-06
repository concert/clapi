{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    DefaultSignatures
  , GADTs
  , MultiParamTypeClasses
  , RankNTypes
  , StandaloneDeriving
  , TemplateHaskell
  , TypeFamilies
  , TypeOperators
#-}

module Clapi.Types.Wire
  ( WireType(..), SomeWireType(..), withWireType
  , wtTime, wtWord32, wtWord64, wtInt32, wtInt64, wtFloat, wtDouble, wtString
  , wtList, wtMaybe, wtPair

  , WireValue(..), wireType, SomeWireValue(..), withWireValue, someWv
  , Wireable(..), wireTypeFor, fromWireable, someWireable, castWireValue

  , WireTypeName(..)

  , getWtShow, getWtEq, getWtOrd
  ) where

import Prelude hiding (fail)

import Control.Monad.Fail (MonadFail(..))
import Data.Bifunctor (bimap)
import Data.Constraint (Dict(..))
import Data.Int
import Data.Proxy
import Data.Text (Text)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import Data.Word
import Language.Haskell.TH (Type(ConT))
import Text.Printf (printf)

import Clapi.Internal.Wire (WireType(..))
import Clapi.Types.Base (Time(..), TypeEnumOf(..))
import Clapi.Types.WireTH (mkGetWtConstraint)
import Clapi.Util (liftRefl, pairRefl)


deriving instance Show (WireType a)

data SomeWireType where
  SomeWireType :: WireType a -> SomeWireType
deriving instance Show SomeWireType

withWireType :: (forall a. WireType a -> r) -> SomeWireType -> r
withWireType f (SomeWireType wt) = f wt

wtTime, wtWord32, wtWord64, wtInt32, wtInt64, wtFloat, wtDouble, wtString
  :: SomeWireType
wtTime = SomeWireType WtTime
wtWord32 = SomeWireType WtWord32
wtWord64 = SomeWireType WtWord64
wtInt32 = SomeWireType WtInt32
wtInt64 = SomeWireType WtInt64
wtFloat = SomeWireType WtFloat
wtDouble = SomeWireType WtDouble
wtString = SomeWireType WtString

wtList :: SomeWireType -> SomeWireType
wtList (SomeWireType wt) = SomeWireType $ WtList wt

wtMaybe :: SomeWireType -> SomeWireType
wtMaybe (SomeWireType wt) = SomeWireType $ WtMaybe wt

wtPair :: SomeWireType -> SomeWireType -> SomeWireType
wtPair (SomeWireType wt1) (SomeWireType wt2) = SomeWireType $ WtPair wt1 wt2

instance TestEquality WireType where
  testEquality WtTime WtTime = Just Refl
  testEquality WtWord32 WtWord32 = Just Refl
  testEquality WtWord64 WtWord64 = Just Refl
  testEquality WtInt32 WtInt32 = Just Refl
  testEquality WtInt64 WtInt64 = Just Refl
  testEquality WtFloat WtFloat = Just Refl
  testEquality WtDouble WtDouble = Just Refl
  testEquality WtString WtString = Just Refl
  testEquality (WtList wt1) (WtList wt2) = liftRefl <$> testEquality wt1 wt2
  testEquality (WtMaybe wt1) (WtMaybe wt2) = liftRefl <$> testEquality wt1 wt2
  testEquality (WtPair wt1x wt1y) (WtPair wt2x wt2y) =
    pairRefl <$> testEquality wt1x wt2x <*> testEquality wt1y wt2y
  testEquality _ _ = Nothing


data WireValue a where
  WireValue :: WireType a -> a -> WireValue a

wireType :: WireValue a -> WireType a
wireType (WireValue wt _) = wt

mkGetWtConstraint "getWtShow" $ ConT ''Show
mkGetWtConstraint "getWtEq" $ ConT ''Eq
mkGetWtConstraint "getWtOrd" $ ConT ''Ord

instance Show (WireValue a) where
  show (WireValue wt a) = case getWtShow wt of
    Dict -> printf "WireValue (%s) %s" (show wt) (show a)

instance Eq (WireValue a) where
  WireValue wt a1 == WireValue _ a2 = case getWtEq wt of
    Dict -> a1 == a2

instance TestEquality WireValue where
  testEquality (WireValue wt1 _) (WireValue wt2 _) = testEquality wt1 wt2


data SomeWireValue where
  SomeWireValue :: WireValue a -> SomeWireValue
deriving instance Show SomeWireValue

withWireValue :: (forall a. WireValue a -> r) -> SomeWireValue -> r
withWireValue f (SomeWireValue wv) = f wv

instance Eq SomeWireValue where
  SomeWireValue wv1 == SomeWireValue wv2 =
    case testEquality wv1 wv2 of
      Nothing -> False
      Just Refl -> wv1 == wv2

someWv :: WireType a -> a -> SomeWireValue
someWv wt a = SomeWireValue $ WireValue wt a


class Wireable a where
  type WT a
  wireTypeFor_ :: proxy a -> WireType (WT a)
  toWt :: a -> WT a
  fromWt :: WT a -> a

  -- Defaults:
  type WT a = a

  default toWt :: (a ~ WT a) => a -> WT a
  toWt = id
  default fromWt :: (a ~ WT a) => WT a -> a
  fromWt = id

instance Wireable Time where
  wireTypeFor_ _ = WtTime
instance Wireable Word32 where
  wireTypeFor_ _ = WtWord32
instance Wireable Word64 where
  wireTypeFor_ _ = WtWord64
instance Wireable Int32 where
  wireTypeFor_ _ = WtInt32
instance Wireable Int64 where
  wireTypeFor_ _ = WtInt64
instance Wireable Float where
  wireTypeFor_ _ = WtFloat
instance Wireable Double where
  wireTypeFor_ _ = WtDouble
instance Wireable Text where
  wireTypeFor_ _ = WtString

instance Wireable a => Wireable [a] where
  type WT [a] = [WT a]
  wireTypeFor_ _ = WtList $ wireTypeFor_ $ Proxy @a
  toWt = fmap toWt
  fromWt = fmap fromWt

instance Wireable a => Wireable (Maybe a) where
  type WT (Maybe a) = Maybe (WT a)
  wireTypeFor_ _ = WtMaybe $ wireTypeFor_ $ Proxy @a
  toWt = fmap toWt
  fromWt = fmap fromWt

instance (Wireable a, Wireable b) => Wireable (a, b) where
  type WT (a, b) = (WT a, WT b)
  wireTypeFor_ _ = WtPair (wireTypeFor_ $ Proxy @a) (wireTypeFor_ $ Proxy @b)
  toWt = bimap toWt toWt
  fromWt = bimap fromWt fromWt

wireTypeFor :: forall a. Wireable a => a -> WireType (WT a)
wireTypeFor _ = wireTypeFor_ $ Proxy @a

fromWireable :: Wireable a => a -> WireValue (WT a)
fromWireable a = WireValue (wireTypeFor a) $ toWt a

-- | Like `someWv`, but if `a` is unambiguous we can automatically pick the
--   correct type witness
someWireable :: Wireable a => a -> SomeWireValue
someWireable = SomeWireValue . fromWireable

castWireValue :: forall m a. (MonadFail m, Wireable a) => SomeWireValue -> m a
castWireValue (SomeWireValue (WireValue wt x)) =
  let targetWt = wireTypeFor_ $ Proxy @a in
    case testEquality wt targetWt of
      Just Refl -> return $ fromWt x
      Nothing -> fail $ printf "Can't cast %s to %s" (show wt) (show targetWt)


data WireTypeName
  = WtnTime
  | WtnWord32 | WtnWord64
  | WtnInt32 | WtnInt64
  | WtnFloat | WtnDouble
  | WtnString
  | WtnList
  | WtnMaybe
  | WtnPair
  deriving (Show, Eq, Ord, Enum, Bounded)

instance TypeEnumOf (WireType a) WireTypeName where
  typeEnumOf = \case
    WtTime -> WtnTime
    WtWord32 -> WtnWord32
    WtWord64 -> WtnWord64
    WtInt32 -> WtnInt32
    WtInt64 -> WtnInt64
    WtFloat -> WtnFloat
    WtDouble -> WtnDouble
    WtString -> WtnString
    WtList {} -> WtnList
    WtMaybe {} -> WtnMaybe
    WtPair {} -> WtnPair

instance TypeEnumOf SomeWireType WireTypeName where
  typeEnumOf = withWireType typeEnumOf

instance TypeEnumOf (WireValue a) WireTypeName where
  typeEnumOf (WireValue wt _) = typeEnumOf wt
