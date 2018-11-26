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
import Text.Printf (printf)

import GHC.TypeLits (Symbol, SomeSymbol(..), symbolVal)

import Clapi.Types.PNat
  (SPNat, SomePNat(..), type (<), (%<), (:<=:))
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

toWord32 :: EnumVal ss -> Word32
toWord32 (EnumVal _ n) = PNat.toWord32 n

-- | Expand the type of an EnumVal to include a larger set of options, whilst
--   guaranteeing that existing values will not change meaning.
upcast
  :: forall ss1 ss2.
  ss1 `IsPrefixOf` ss2
  => EnumVal ss1 -> SymbolList ss2 -> EnumVal ss2
upcast (EnumVal _ sPNat) sl =
    case blah (prefixProvesLte $ prefixProof @ss1 @ss2) sPNat of
      Dict -> EnumVal sl sPNat
  where
    blah
      :: forall n. n < Length ss1
      => Length ss1 :<=: Length ss2 -> SPNat n -> Dict (n < Length ss2)
    blah lteProof _ = PNat.ltDict $
      PNat.extendLt (PNat.proofLT @n @(Length ss1)) lteProof

-- | Change the type of an EnumVal to a different set of options, making sure
--   that _this_ value doesn't change it's meaning
castValue
  :: forall m ss1 ss2. MonadFail m
  => EnumVal ss1 -> SymbolList ss2 -> m (EnumVal ss2)
castValue (EnumVal sl1 n) sl2 = case n %< SL.length sl2 of
    Nothing -> fail $ printf "Value outside range %s" (show $ SL.toStrings sl2)
    Just proof -> case PNat.ltDict proof of
      Dict -> let v1 = sl1 !! n; v2 = sl2 !! n in
        if v1 == v2
        then return $ EnumVal sl2 n
        else fail $
          printf "Cannot cast value '%s' to '%s'" (symString v1) (symString v2)
  where
    symString :: SomeSymbol -> String
    symString (SomeSymbol p) = symbolVal p
