{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Clapi.Attributor where

import Control.Monad (forever)

import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.Types (Attributee, TrDigest(..), TrcDigest(..), DataChange(..))

attributor
  :: (Monad m, Functor f)
  => Attributee -> Protocol (f TrDigest) (f TrDigest) a a m ()
attributor u = forever $ waitThen (sendFwd . fmap attributeClient) sendRev
  where
    attributeClient (Trcd d) = Trcd $ d{
      trcdReorderings = fmap modPairFst <$> trcdReorderings d,
      trcdData = attributeDc <$> trcdData d}
    attributeClient d = d
    attributeDc dc = case dc of
      InitChange ma -> InitChange $ modAttr ma
      DeleteChange ma -> DeleteChange $ modAttr ma
      ConstChange ma vs -> ConstChange (modAttr ma) vs
      TimeChange m -> TimeChange $ modPairFst <$> m
    modPairFst (ma, x) = (modAttr ma, x)
    modAttr = Just . maybe u id
