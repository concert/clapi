{-# LANGUAGE
    AllowAmbiguousTypes
  , ExistentialQuantification
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , PolyKinds
  , Rank2Types
  , StandaloneDeriving
  , TemplateHaskell
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

import Prelude hiding (fail)

import Control.Monad.Fail (MonadFail(..))
import Data.Bifunctor (bimap)
import Data.Int
import Data.Text (Text)
import Data.Word
import Data.Type.Equality (TestEquality(..), (:~:))
import Data.Typeable
import Data.Maybe (fromJust)

import Clapi.Serialisation.Base (Encodable)
import Clapi.Types.Base (Time(..))
import Clapi.Types.WireTH (mkWithWtProxy)
import Clapi.Util (proxyF, proxyF3)

-- data NewWireValue a where
--   NwvTime :: Time -> NewWireValue Time
--   NwvWord32 :: Word32 -> NewWireValue Word32
--   NwvWord64 :: Word64 -> NewWireValue Word64
--   NwvInt32 :: Int32 -> NewWireValue Int32
--   NwvInt64 :: Int64 -> NewWireValue Int64
--   NwvFloat :: Float -> NewWireValue Float
--   NwvDouble :: Double -> NewWireValue Double
--   NwvText :: Text -> NewWireValue Text
--   NwvList :: [x] -> NewWireValue [x]
--   NwvMaybe :: Maybe x -> NewWireValue (Maybe x)
--   NwvPair :: (x, y) -> NewWireValue (x, y)

data SomeWireable where
  SomeWireable :: NewWireable a => a -> SomeWireable


awtRefl :: AnotherWT a :~: AnotherWT b -> a :~: b
awtRefl Refl = Refl

instance Eq SomeWireable where
  SomeWireable wv1 == SomeWireable wv2 = awtyEq' wv1 wv2

instance Ord SomeWireable where
  SomeWireable wv1 `compare` SomeWireable wv2 = undefined

-- nwvList :: [NewWireValue x] -> NewWireValue [x]
-- nwvList = NwvList . fmap unwrapNwv

-- nwvMaybe :: Maybe (NewWireValue x) -> NewWireValue (Maybe x)
-- nwvMaybe = NwvMaybe . fmap unwrapNwv

-- nwvPair :: (NewWireValue x, NewWireValue y) -> NewWireValue (x, y)
-- nwvPair = NwvPair . bimap unwrapNwv unwrapNwv

class (Eq a, Ord a) => NewWireable a where
  toWireType :: proxy a -> WireType
  toAnotherWireType :: AnotherWT a

instance NewWireable Time where
  toWireType _ = WtTime
  toAnotherWireType = AwtTime

instance NewWireable Word32 where
  toWireType _ = WtWord32
  toAnotherWireType = undefined

instance NewWireable a => NewWireable [a] where
  toWireType _ = WtList $ toWireType $ Proxy @a
  toAnotherWireType = AwtList $ toAnotherWireType @a

instance NewWireable a => NewWireable (Maybe a) where
  toWireType _ = WtMaybe $ toWireType $ Proxy @a
  toAnotherWireType = undefined

instance (NewWireable a, NewWireable b) => NewWireable (a, b) where
  toWireType _ = WtPair (toWireType $ Proxy @a) (toWireType $ Proxy @b)
  toAnotherWireType = AwtPair
    (toAnotherWireType @a)
    (toAnotherWireType @b)


data AnotherWT a where
  AwtTime :: AnotherWT Time
  AwtList :: AnotherWT a -> AnotherWT [a]
  AwtPair :: AnotherWT x -> AnotherWT y -> AnotherWT (x, y)

deriving instance Eq (AnotherWT a)

data AwtConstructor = AwtTimeC | AwtListC | AwtPairC deriving (Eq, Ord)

awtToC :: AnotherWT a -> AwtConstructor
awtToC = \case
  AwtTime -> AwtTimeC
  AwtList _ -> AwtListC
  AwtPair _ _ -> AwtPairC

instance Ord (AnotherWT a) where
  compare AwtTime AwtTime = EQ
  compare (AwtList awt1) (AwtList awt2) = compare awt1 awt2
  compare (AwtPair awt1x awt1y) (AwtPair awt2x awt2y) =
    compare (awt1x, awt1y) (awt2x, awt2y)
  compare awt1 awt2 = compare (awtToC awt1) (awtToC awt2)

data SomeAwt where
  SomeAwt :: AnotherWT a -> SomeAwt

instance TestEquality AnotherWT where
  testEquality AwtTime AwtTime = Just Refl
  testEquality (AwtList awt1) (AwtList awt2) = snarf <$> testEquality awt1 awt2
  testEquality (AwtPair awt1x awt1y) (AwtPair awt2x awt2y) =
    reflPair <$> testEquality awt1x awt2x <*> testEquality awt1y awt2y
  testEquality _ _ = Nothing


awtyEq' :: forall a b. (Eq a, NewWireable a, NewWireable b) => a -> b -> Bool
awtyEq' a b = awtyEq (toAnotherWireType @a) a (toAnotherWireType @b) b

awtyEq :: Eq a => AnotherWT a -> a -> AnotherWT b -> b -> Bool
awtyEq awt1 a awt2 b = case testEquality awt1 awt2 of
  Nothing -> False
  Just Refl -> a == b

awtyOrd :: Ord a => AnotherWT a -> a -> AnotherWT b -> b -> Ordering
awtyOrd awt1 a awt2 b = case testEquality awt1 awt2 of
  Nothing -> undefined
  Just Refl -> a `compare` b


snarf :: a :~: b -> f a :~: f b
snarf Refl = Refl

reflPair :: a1 :~: a2 -> b1 :~: b2 -> (a1, b1) :~: (a2, b2)
reflPair Refl Refl = Refl


data WireType
  = WtTime
  | WtWord32 | WtWord64
  | WtInt32 | WtInt64
  | WtFloat | WtDouble
  | WtString
  | WtList WireType
  | WtMaybe WireType
  | WtPair WireType WireType
  deriving (Show, Eq, Ord)
