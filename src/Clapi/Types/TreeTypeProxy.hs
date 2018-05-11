{-# LANGUAGE
    KindSignatures
  , PolyKinds
  , Rank2Types
  , ScopedTypeVariables
  , TypeApplications
#-}

module Clapi.Types.TreeTypeProxy
  ( withTtProxy'
  ) where

import Data.Int
import Data.Proxy
import Data.Text (Text)
import Data.Word

import Clapi.Types.Base (Time)
import Clapi.Types.Wire (Wireable)
import Clapi.Types.Tree (TreeType'(..))


withTtProxy' :: forall r. TreeType' -> (forall (a :: *). Wireable a => Proxy a -> r) -> r
withTtProxy' tt f = case tt of
    TtTime -> f $ Proxy @Time
    TtEnum _ -> f $ Proxy @Word8
    TtWord32 _ -> f $ Proxy @Word32
    TtWord64 _ -> f $ Proxy @Word64
    TtInt32 _ -> f $ Proxy @Int32
    TtInt64 _ -> f $ Proxy @Int64
    TtFloat _ -> f $ Proxy @Float
    TtDouble _ -> f $ Proxy @Double
    TtString _ -> f $ Proxy @Text
    TtRef _ -> f $ Proxy @Text
    TtList tt' -> listy tt'
    TtSet tt' -> listy tt'
    TtOrdSet tt' -> listy tt'
    TtMaybe tt' -> withTtProxy' tt' (f . proxyF (Proxy @Maybe))
    TtPair tt1 tt2 ->
      withTtProxy' tt1 $ \pConc1 ->
        withTtProxy' tt2 $ \pConc2 -> f $ proxyF3 (Proxy @(,)) pConc1 pConc2
  where
    listy tt' = withTtProxy' tt' (f. proxyF (Proxy @[]))

proxyF :: Proxy a -> Proxy b -> Proxy (a b)
proxyF _ _ = Proxy

proxyF3 :: Proxy a -> Proxy b -> Proxy c -> Proxy (a b c)
proxyF3 p1 p2 p3 = proxyF (proxyF p1 p2) p3
