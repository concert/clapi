{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Clapi.Attributor where

import Control.Monad (forever)
import Data.Bifunctor (first)

import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.Types
  (Attributee, TrDigest(..), TrcUpdateDigest(..), DataChange(..))

attributor
  :: (Monad m, Functor f)
  => Attributee -> Protocol (f TrDigest) (f TrDigest) a a m ()
attributor u = forever $ waitThen (sendFwd . fmap attributeClient) sendRev
  where
    attributeClient (Trcud d) = Trcud $ d{
      trcudContOps = fmap (first modAttr) <$> trcudContOps d,
      trcudData = attributeDc <$> trcudData d}
    attributeClient d = d
    attributeDc dc = case dc of
      ConstChange ma vs -> ConstChange (modAttr ma) vs
      TimeChange m -> TimeChange $ first modAttr <$> m
    modAttr = Just . maybe u id
