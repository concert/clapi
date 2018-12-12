{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , RankNTypes
  , TypeOperators
  , StandaloneDeriving
#-}

module Clapi.Types.Symbol where

import Data.Proxy
import Data.Type.Equality (TestEquality(..), (:~:)(..))

import GHC.TypeLits
  (Symbol, KnownSymbol, SomeSymbol(..), someSymbolVal, symbolVal, sameSymbol)


withKnownSymbol :: (forall s. KnownSymbol s => Proxy s -> r) -> String -> r
withKnownSymbol f s = case someSymbolVal s of
  SomeSymbol p -> f p


-- | SSymbol is just a re-implementation of Data.Constraint.Dict, sepcifically
--   for representing Symbol types.
data SSymbol (s :: Symbol) where
  SSymbol :: KnownSymbol s => SSymbol s

instance Show (SSymbol s) where
  showsPrec p s = showParen (p >= 11) $
      showString "fromString "
    . showsPrec p (toString s)

instance Eq (SSymbol s) where
  _ == _ = True

instance Ord (SSymbol s) where
  compare _ _ = EQ

instance TestEquality SSymbol where
  testEquality = sameSSymbol

sameSSymbol :: forall s1 s2. SSymbol s1 -> SSymbol s2 -> Maybe (s1 :~: s2)
sameSSymbol SSymbol SSymbol = sameSymbol (Proxy @s1) (Proxy @s2)

toString :: forall s. SSymbol s -> String
toString SSymbol = symbolVal $ Proxy @s

data SomeSSymbol where
  SomeSSymbol :: SSymbol s -> SomeSSymbol
deriving instance Show SomeSSymbol

instance Eq SomeSSymbol where
  SomeSSymbol s1 == SomeSSymbol s2 = toString s1 == toString s2

instance Ord SomeSSymbol where
  compare (SomeSSymbol s1) (SomeSSymbol s2) =
    compare (toString s1) (toString s2)


withSSymbol :: (forall s. SSymbol s -> r) -> SomeSSymbol -> r
withSSymbol f (SomeSSymbol s) = f s

fromString :: String -> SomeSSymbol
fromString s = case someSymbolVal s of SomeSymbol p -> SomeSSymbol $ go p
  where
    go :: KnownSymbol s => Proxy s -> SSymbol s
    go _ = SSymbol

withSSymbolFromString :: (forall s. SSymbol s -> r) -> String -> r
withSSymbolFromString f = withSSymbol f . fromString
