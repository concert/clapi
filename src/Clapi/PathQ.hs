{-# LANGUAGE
    QuasiQuotes
  , TemplateHaskell
  , StandaloneDeriving
  , DeriveLift
#-}

module Clapi.PathQ (pathq) where

import Control.Monad ((>=>))
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (Lift, lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Clapi.Path (Path(..))
import Path.Parsing (fromString)

deriving instance Lift Path

pathq :: QuasiQuoter
pathq = QuasiQuoter {
    quoteExp = fromString >=> lift,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}
