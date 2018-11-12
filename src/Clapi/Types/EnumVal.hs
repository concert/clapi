{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , TypeOperators
  , StandaloneDeriving
#-}
module Clapi.Types.EnumVal where

import Prelude hiding (fail, (!!))
import Control.Monad.Fail (MonadFail(..))
import Data.Type.Equality ((:~:)(..))
import Data.Word

import GHC.TypeLits (Symbol, SomeSymbol(..), symbolVal)

import Clapi.Types.PNat (SPNat, SomePNat(..), (:<), (%<))
import qualified Clapi.Types.PNat as PNat
import Clapi.Types.SymbolList (SymbolList, Length, (!!))
import qualified Clapi.Types.SymbolList as SL

data EnumVal (ss :: [Symbol]) where
  EnumVal :: n :< Length ss ~ 'True => SymbolList ss -> SPNat n -> EnumVal ss

instance Show (EnumVal ss) where
  show ev = "<EnumVal " ++ enumName ev ++ ">"

data SomeEnumVal where
  SomeEnumVal :: EnumVal ss -> SomeEnumVal

instance Show SomeEnumVal where
  show (SomeEnumVal ev) = "<SomeEnumVal " ++ enumName ev ++ ">"

enumName :: EnumVal ss -> String
enumName (EnumVal sl n) = case sl !! n of SomeSymbol p -> symbolVal p

enumVal :: MonadFail m => SymbolList ss -> Word32 -> m (EnumVal ss)
enumVal sl w = case PNat.fromWord32 w of
  SomePNat sPNat -> case sPNat %< SL.length sl of
    Nothing -> fail "darf"
    Just Refl -> return $ EnumVal sl sPNat

enumVal_ :: MonadFail m => [String] -> Word32 -> m SomeEnumVal
enumVal_ ss w = SL.withSymbolList (fmap SomeEnumVal . flip enumVal w) ss
