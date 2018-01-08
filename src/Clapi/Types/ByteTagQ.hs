{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    QuasiQuotes
  , TemplateHaskell
  , ScopedTypeVariables
#-}
module Clapi.Types.ByteTagQ (btq) where

import Control.Monad ((>=>))
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as MF
import Data.Word
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Clapi.Util (bound)

btq :: QuasiQuoter
btq = QuasiQuoter {
    quoteExp = fromStr >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}
  where
    fromStr [c] = fromChar c
    fromStr _ = fail "Not one char"
    fromChar :: MonadFail m => Char -> m Word8
    fromChar = bound
