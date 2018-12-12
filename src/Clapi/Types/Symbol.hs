{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , RankNTypes
  , TypeOperators
#-}

module Clapi.Types.Symbol where

import Data.Proxy
import Data.Type.Equality ((:~:)(..))

import GHC.TypeLits
  (Symbol, KnownSymbol, SomeSymbol(..), someSymbolVal, symbolVal, sameSymbol)


withKnownSymbol :: (forall s. KnownSymbol s => Proxy s -> r) -> String -> r
withKnownSymbol f s = case someSymbolVal s of
  SomeSymbol p -> f p


-- | SSymbol is just a re-implementation of Data.Constraint.Dict, sepcifically
--   for representing Symbol types.
data SSymbol (s :: Symbol) where
  SSymbol :: KnownSymbol s => SSymbol s

sameSSymbol :: forall s1 s2. SSymbol s1 -> SSymbol s2 -> Maybe (s1 :~: s2)
sameSSymbol SSymbol SSymbol = sameSymbol (Proxy @s1) (Proxy @s2)

toString :: forall s. SSymbol s -> String
toString SSymbol = symbolVal $ Proxy @s

data SomeSSymbol where
  SomeSSymbol :: SSymbol s -> SomeSSymbol

withSSymbol :: (forall s. SSymbol s -> r) -> SomeSSymbol -> r
withSSymbol f (SomeSSymbol s) = f s

fromString :: String -> SomeSSymbol
fromString s = case someSymbolVal s of SomeSymbol p -> SomeSSymbol $ go p
  where
    go :: KnownSymbol s => Proxy s -> SSymbol s
    go _ = SSymbol

withSSymbolFromString :: (forall s. SSymbol s -> r) -> String -> r
withSSymbolFromString f = withSSymbol f . fromString
