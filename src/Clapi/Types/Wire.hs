{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DataKinds
  , ExistentialQuantification
  , FlexibleInstances
  , GADTs
  , InstanceSigs
  , LambdaCase
  , MultiParamTypeClasses
  , PolyKinds
  , Rank2Types
  , StandaloneDeriving
  , TemplateHaskell
  , TypeFamilies
  , TypeFamilyDependencies
  , TypeInType
  , TypeOperators
#-}

module Clapi.Types.Wire
  -- ( -- Wireable
  -- -- , WireValue(..), castWireValue
  -- -- , (<|$|>), (<|*|>)
  -- -- , cast'
  -- WireType(..) --, wireValueWireType, withWtProxy, withWvValue

  -- , NewWireValue(..), NewWireable(..), SomeWireable(..), unwrapNwv
  -- ) where
  where

import Data.Constraint (Dict(..), mapDict, (:=>)(..), (:-)(..))
import Data.Int
import Data.Text (Text)
import Data.Word
import Data.Type.Equality (TestEquality(..), (:~:))
import Data.Typeable

import Clapi.Types.Base (Time(..))

data WireType a where
  WtTime :: WireType Time
  WtWord32 :: WireType Word32
  WtWord64 :: WireType Word64
  WtInt32 :: WireType Int32
  WtInt64 :: WireType Int64
  WtFloat :: WireType Float
  WtDouble :: WireType Double
  WtString :: WireType Text
  WtList :: WireType x -> WireType [x]
  WtMaybe :: WireType x -> WireType (Maybe x)
  WtPair :: WireType x -> WireType y -> WireType (x, y)

deriving instance Show (WireType a)

instance TestEquality WireType where
  testEquality WtTime WtTime = Just Refl
  testEquality WtWord32 WtWord32 = Just Refl
  testEquality (WtList wt1) (WtList wt2) = liftRefl <$> testEquality wt1 wt2
  testEquality (WtPair wt1x wt1y) (WtPair wt2x wt2y) =
    pairRefl <$> testEquality wt1x wt2x <*> testEquality wt1y wt2y
  testEquality _ _ = Nothing

liftRefl :: a :~: b -> f a :~: f b
liftRefl Refl = Refl

pairRefl :: a :~: b -> c :~: d -> (a, c) :~: (b, d)
pairRefl Refl Refl = Refl

getEq :: WireType a -> Dict (Eq a)
getEq = \case
  WtTime -> Dict
  WtWord32 -> Dict
  WtWord64 -> Dict
  WtInt32 -> Dict
  WtInt64 -> Dict
  WtFloat -> Dict
  WtDouble -> Dict
  WtString -> Dict
  WtList wt -> mapDict ins $ getEq wt
  WtMaybe wt -> mapDict ins $ getEq wt
  WtPair wt1 wt2 -> pairDictEq (getEq wt1) (getEq wt2)

pairDictEq :: Dict (Eq a) -> Dict (Eq b) -> Dict (Eq (a, b))
pairDictEq Dict Dict = Dict

getShow :: WireType a -> Dict (Show a)
getShow = \case
  WtTime -> Dict
  WtWord32 -> Dict
  WtWord64 -> Dict
  WtInt32 -> Dict
  WtInt64 -> Dict
  WtFloat -> Dict
  WtDouble -> Dict
  WtString -> Dict
  WtList wt -> mapDict ins $ getShow wt
  WtMaybe wt -> mapDict ins $ getShow wt
  WtPair wt1 wt2 -> pairDictShow (getShow wt1) (getShow wt2)

pairDictShow :: Dict (Show a) -> Dict (Show b) -> Dict (Show (a, b))
pairDictShow Dict Dict = Dict

data SomeWireType where
  SomeWireType :: WireType a -> SomeWireType

data WireValue where
  WireValue :: WireType a -> a -> WireValue

instance Show WireValue where
  show (WireValue wt a) = case getShow wt of
    Dict -> show a

instance Eq WireValue where
  WireValue wt1 a1 == WireValue wt2 a2 = case testEquality wt1 wt2 of
    Nothing -> False
    Just Refl -> case getEq wt1 of
      Dict -> a1 == a2

class Wireable a where
  wireTypeFor :: proxy a -> WireType a

instance Wireable Time where
  wireTypeFor _ = WtTime
instance Wireable Word32 where
  wireTypeFor _ = WtWord32
instance Wireable Word64 where
  wireTypeFor _ = WtWord64
instance Wireable Int32 where
  wireTypeFor _ = WtInt32
instance Wireable Int64 where
  wireTypeFor _ = WtInt64
instance Wireable Float where
  wireTypeFor _ = WtFloat
instance Wireable Double where
  wireTypeFor _ = WtDouble
instance Wireable Text where
  wireTypeFor _ = WtString
instance Wireable a => Wireable [a] where
  wireTypeFor _ = WtList $ wireTypeFor $ Proxy @a
instance Wireable a => Wireable (Maybe a) where
  wireTypeFor _ = WtMaybe $ wireTypeFor $ Proxy @a
instance (Wireable a, Wireable b) => Wireable (a, b) where
  wireTypeFor _ = WtPair (wireTypeFor $ Proxy @a) (wireTypeFor $ Proxy @b)


wireTypeOf :: Wireable a => a -> WireType a
wireTypeOf _ = wireTypeFor Proxy

wireValue :: Wireable a => a -> WireValue
wireValue a = WireValue (wireTypeOf a) a

getDict :: (c Time, c Time :=> c [Time]) => WireType a -> Dict (c a)
getDict = \case
  WtTime -> Dict
  WtList wt -> undefined
