{-# LANGUAGE
    TemplateHaskell
#-}

module Clapi.TH (nameq, pathq, btq) where

import Control.Monad ((>=>))
import Data.Char (ord)
import Data.Text (pack)
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Clapi.Types.Base (mkTag)
import Clapi.Types.Path (fromText, mkSeg, segP)

nameq :: QuasiQuoter
nameq = QuasiQuoter {
    quoteExp = mkSeg . pack >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}

pathq :: QuasiQuoter
pathq = QuasiQuoter {
    quoteExp = fromText segP . pack >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}

btq :: QuasiQuoter
btq = QuasiQuoter {
    quoteExp = fromStr >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}
  where
    fromStr [c] = mkTag $ fromIntegral $ ord c
    fromStr _ = fail "Not one char"
