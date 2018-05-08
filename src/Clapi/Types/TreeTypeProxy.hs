{-# LANGUAGE
    KindSignatures
  , PolyKinds
  , Rank2Types
  , ScopedTypeVariables
  , TypeApplications
#-}

module Clapi.Types.TreeTypeProxy
  ( withTtProxy
  ) where

import Data.Proxy
import Data.Word

import Clapi.Types.Wire (Wireable)
import Clapi.Types.Tree
  (TreeType(..), TreeConcreteType(..), TreeContainerType(..))


withTtProxy :: TreeType -> (forall (a :: *). Wireable a => Proxy a -> r) -> r
withTtProxy tt f = case tt of
  TtConc tct -> withTtConcProxy tct f
  TtCont tct -> withTtContProxy tct f

withTtConcProxy
  :: TreeConcreteType -> (forall (a :: *). Wireable a => Proxy a -> r) -> r
withTtConcProxy tct f = case tct of
  TcWord32 _ -> f $ Proxy @Word32
  -- FIXME: etc

withTtContProxy
  :: TreeContainerType -> (forall (a :: *). Wireable a => Proxy a -> r) -> r
withTtContProxy tct f = case tct of
  TcList tt -> withTtProxy tt (\pConc -> f $ proxyF (Proxy @[]) pConc)
  -- FIXME: etc...
  TcPair tt1 tt2 ->
    withTtProxy tt1 $ \pConc1 ->
      withTtProxy tt2 $ \pConc2 -> f $ proxyF3 (Proxy @(,)) pConc1 pConc2

proxyF :: Proxy a -> Proxy b -> Proxy (a b)
proxyF _ _ = Proxy

proxyF3 :: Proxy a -> Proxy b -> Proxy c -> Proxy (a b c)
proxyF3 p1 p2 p3 = proxyF (proxyF p1 p2) p3
