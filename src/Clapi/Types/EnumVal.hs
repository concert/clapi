{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , GADTs
  , KindSignatures
  , TypeOperators
#-}

module Clapi.Types.EnumVal where

import Prelude hiding ((!!), fail)

import Control.Monad.Fail (MonadFail(..))
import Data.Constraint (Dict(..))
import Data.Word
import Text.Printf (printf)

import GHC.TypeLits (Symbol)

import Clapi.Types.Nat (SNat, SomeSNat(..), type (<), (%<), (:<=:))
import qualified Clapi.Types.Nat as Nat
import Clapi.Types.Symbol (SomeSSymbol(..), withSSymbol)
import qualified Clapi.Types.Symbol as SSymbol
import Clapi.Types.SymbolList
  (SymbolList, Length, (!!), withSymbolListFromStrings)
import qualified Clapi.Types.SymbolList as SL


data EnumVal (ss :: [Symbol]) where
  EnumVal :: n < Length ss => SymbolList ss -> SNat n -> EnumVal ss

instance Show (EnumVal ss) where
  show ev = "<EnumVal " ++ enumName ev ++ ">"

instance Eq (EnumVal ss) where
  EnumVal _ m == EnumVal _ n = Nat.toWord32 m == Nat.toWord32 n

instance Ord (EnumVal ss) where
  compare (EnumVal _ m) (EnumVal _ n) =
    compare (Nat.toWord32 m) (Nat.toWord32 n)

enumSSymbol :: EnumVal ss -> SomeSSymbol
enumSSymbol (EnumVal sl n) = SomeSSymbol $ sl !! n

enumName :: EnumVal ss -> String
enumName = withSSymbol SSymbol.toString . enumSSymbol

toWord32 :: EnumVal ss -> Word32
toWord32 (EnumVal _ n) = Nat.toWord32 n


data SomeEnumVal where
  SomeEnumVal :: EnumVal ss -> SomeEnumVal

instance Show SomeEnumVal where
  show (SomeEnumVal ev) = "<SomeEnumVal " ++ enumName ev ++ ">"


enumVal :: MonadFail m => SymbolList ss -> Word32 -> m (EnumVal ss)
enumVal sl w = case Nat.fromWord32 w of
  SomeSNat n -> case n %< SL.length sl of
    Nothing -> fail $ show n ++ " out of range of enumeration"
    Just lt -> case Nat.ltDict lt of
      Dict -> return $ EnumVal sl n

enumVal_ :: MonadFail m => [String] -> Word32 -> m SomeEnumVal
enumVal_ ss w = withSymbolListFromStrings (fmap SomeEnumVal . flip enumVal w) ss


-- | Expand the type of an EnumVal to include a larger set of options, whilst
--   guaranteeing that existing values will not change meaning.
upcast
  :: forall ss1 ss2. ss1 `SL.IsPrefixOf` ss2
  => EnumVal ss1 -> SymbolList ss2 -> EnumVal ss2
upcast (EnumVal _ n) sl =
    case getProof (SL.prefixProvesLte $ SL.prefixProof @ss1 @ss2) n of
      Dict -> EnumVal sl n
  where
    getProof
      :: forall n. n < Length ss1
      => Length ss1 :<=: Length ss2 -> SNat n -> Dict (n < Length ss2)
    getProof lteProof _ = Nat.ltDict $
      Nat.extendLt (Nat.proofLT @n @(Length ss1)) lteProof

-- | Change the type of an EnumVal to a different set of options, making sure
--   that _this_ value doesn't change it's meaning.
castValue
  :: forall m ss1 ss2. MonadFail m
  => EnumVal ss1 -> SymbolList ss2 -> m (EnumVal ss2)
castValue (EnumVal sl1 n) sl2 = case n %< SL.length sl2 of
  Nothing -> fail $ "Value outside range " ++ show (SL.toStrings sl2)
  Just proof -> case Nat.ltDict proof of
    Dict -> let
        s1 = SSymbol.toString $ sl1 !! n
        s2 = SSymbol.toString $ sl2 !! n
      in if s1 == s2
        then return $ EnumVal sl2 n
        else fail $ printf "Cannot cast value '%s' to '%s'" s1 s2
