{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    QuasiQuotes
  , TemplateHaskell
  , ScopedTypeVariables
#-}
module Clapi.Types.ByteTagQ (btq) where

import Control.Monad ((>=>))
import Data.Char (ord)
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Clapi.Types.Base (mkTag)

btq :: QuasiQuoter
btq = QuasiQuoter {
    quoteExp = fromStr >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}
  where
    fromStr [c] = mkTag $ fromIntegral $ ord c
    fromStr _ = fail "Not one char"
