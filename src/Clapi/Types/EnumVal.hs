{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , GADTs
  , KindSignatures
  , TypeOperators
  , StandaloneDeriving
#-}
module Clapi.Types.EnumVal where

import Prelude hiding (fail, (!!))
import Control.Monad.Fail (MonadFail(..))
import Data.Constraint (Dict(..))
import Data.Word

import GHC.TypeLits (Symbol, SomeSymbol(..), symbolVal)

import Clapi.Types.PNat
  (SPNat, SomePNat(..), type (<), (%<))
import qualified Clapi.Types.PNat as PNat
import Clapi.Types.SymbolList
  (SymbolList, Length, (!!), IsPrefixOf(..), prefixProvesLte)
import qualified Clapi.Types.SymbolList as SL

data EnumVal (ss :: [Symbol]) where
  EnumVal :: n < Length ss => SymbolList ss -> SPNat n -> EnumVal ss

instance Show (EnumVal ss) where
  show ev = "<EnumVal " ++ enumName ev ++ ">"

instance Eq (EnumVal ss) where
  EnumVal _ m == EnumVal _ n = PNat.toWord32 m == PNat.toWord32 n

instance Ord (EnumVal ss) where
  compare (EnumVal _ m) (EnumVal _ n) =
    compare (PNat.toWord32 m) (PNat.toWord32 n)

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
    Just proof -> case PNat.ltDict proof of
      Dict -> return $ EnumVal sl sPNat

enumVal_ :: MonadFail m => [String] -> Word32 -> m SomeEnumVal
enumVal_ ss w = SL.withSymbolList (fmap SomeEnumVal . flip enumVal w) ss
