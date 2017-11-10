{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Clapi.PathQ (pathq) where
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Data.Maybe (fromJust)

import Path.Parsing (fromString)

qe :: String -> Q Exp
qe ps = fromString ps >>= const [| fromJust $ fromString ps |]

pathq :: QuasiQuoter
pathq = QuasiQuoter {
    quoteExp = qe,
    quotePat = fail "Not supported",
    quoteDec = fail "Not supported",
    quoteType = fail "Not supported"}
