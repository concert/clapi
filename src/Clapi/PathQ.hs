{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    QuasiQuotes
  , TemplateHaskell
#-}

module Clapi.PathQ (pathq, segq) where

import Control.Monad ((>=>))
import Data.Text (pack)
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Clapi.Path (fromText, mkSeg)

pathq :: QuasiQuoter
pathq = QuasiQuoter {
    quoteExp = fromText . pack >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}

segq :: QuasiQuoter
segq = QuasiQuoter {
    quoteExp = mkSeg . pack >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}
