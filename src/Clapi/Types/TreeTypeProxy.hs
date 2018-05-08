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

import Data.Int
import Data.Proxy
import Data.Text (Text)
import Data.Word

import Clapi.Types.Base (Time)
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
  TcTime -> f $ Proxy @Time
  TcEnum _ -> f $ Proxy @Word8
  TcWord32 _ -> f $ Proxy @Word32
  TcWord64 _ -> f $ Proxy @Word64
  TcInt32 _ -> f $ Proxy @Int32
  TcInt64 _ -> f $ Proxy @Int64
  TcFloat _ -> f $ Proxy @Float
  TcDouble _ -> f $ Proxy @Double
  TcString _ -> f $ Proxy @Text
  TcRef _ -> f $ Proxy @Text
  TcValidatorDesc -> f $ Proxy @Text

withTtContProxy
  :: TreeContainerType -> (forall (a :: *). Wireable a => Proxy a -> r) -> r
withTtContProxy tct f = case tct of
    TcList tt -> listy tt
    TcSet tt -> listy tt
    TcOrdSet tt -> listy tt
    TcMaybe tt -> withTtProxy tt (\pConc -> f $ proxyF (Proxy @Maybe) pConc)
    TcPair tt1 tt2 ->
      withTtProxy tt1 $ \pConc1 ->
        withTtProxy tt2 $ \pConc2 -> f $ proxyF3 (Proxy @(,)) pConc1 pConc2
  where
    listy tt = withTtProxy tt (\pConc -> f $ proxyF (Proxy @[]) pConc)

proxyF :: Proxy a -> Proxy b -> Proxy (a b)
proxyF _ _ = Proxy

proxyF3 :: Proxy a -> Proxy b -> Proxy c -> Proxy (a b c)
proxyF3 p1 p2 p3 = proxyF (proxyF p1 p2) p3
