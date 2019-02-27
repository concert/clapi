{-# LANGUAGE
    TemplateHaskell
#-}

module Clapi.TH (n, pathq, btq) where

import Control.Monad ((>=>))
import Data.Char (ord)
import Data.Text (pack)
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Clapi.Types.Base (mkTag)
import Clapi.Types.Name (mkName, nameP)
import Clapi.Types.Path (fromText)

n :: QuasiQuoter
n = QuasiQuoter {
    quoteExp = mkName . pack >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}

pathq :: QuasiQuoter
pathq = QuasiQuoter {
    quoteExp = fromText nameP . pack >=> lift,
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
