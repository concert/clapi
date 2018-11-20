{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , KindSignatures
  , LambdaCase
  , MultiParamTypeClasses
  , RankNTypes
  , TypeFamilies
  , TypeOperators
  , StandaloneDeriving
#-}

module Clapi.Types.PNat where

import Data.Constraint (Dict(..))
import Data.Type.Equality ((:~:)(..))
import Data.Word

-- | A regular data type for the Peano numbers that we can lift to type level
data PNat = Zero | Succ PNat

-- | Singleton witnesses for type level PNats
data SPNat (n :: PNat) where
  SPZero :: SPNat 'Zero
  SPSucc :: SPNat n -> SPNat ('Succ n)

toWord32 :: SPNat a -> Word32
toWord32 = \case
  SPZero -> 0
  SPSucc sPNat -> 1 + toWord32 sPNat

instance Show (SPNat a) where
  show = show . toWord32

withSPNat :: forall r. (forall n. SPNat n -> r) -> Word32 -> r
withSPNat f = go SPZero
  where
    go :: SPNat n -> Word32 -> r
    go sPNat 0 = f sPNat
    go sPNat n = go (SPSucc sPNat) (n - 1)

-- | Existential wrapper for any PNat singleton witness to hide the type var
data SomePNat where
  SomePNat :: SPNat n -> SomePNat
deriving instance Show SomePNat

fromWord32 :: Word32 -> SomePNat
fromWord32 = withSPNat SomePNat

data (:<:) (m :: PNat) (n :: PNat) where
  LT1 :: 'Zero :<: 'Succ n
  LT2 :: m :<: n -> 'Succ m :<: 'Succ n
deriving instance Show (m :<: n)

(%<) :: SPNat m -> SPNat n -> Maybe (m :<: n)
_ %< SPZero = Nothing
SPZero %< SPSucc n = Just LT1
SPSucc m %< SPSucc n = LT2 <$> m %< n

class (m :: PNat) < (n :: PNat) where
  proofLT :: m :<: n

instance 'Zero < 'Succ n where
  proofLT = LT1

instance m < n => 'Succ m < 'Succ n where
  proofLT = LT2 proofLT

ltDict :: m :<: n -> Dict (m < n)
ltDict LT1 = Dict
ltDict (LT2 proof) = case ltDict proof of Dict -> Dict

data (:<=:) (m :: PNat) (n :: PNat) where
  LTE1 :: 'Zero :<=: n
  LTE2 :: m :<=: n -> 'Succ m :<=: 'Succ n
deriving instance Show (m :<=: n)

(%<=) :: SPNat m -> SPNat n -> Maybe (m :<=: n)
SPZero %<= n = Just LTE1
SPSucc m %<= SPZero = Nothing
SPSucc m %<= SPSucc n = LTE2 <$> m %<= n

class (m :: PNat) <= (n :: PNat) where
  proofLTE :: m :<=: n

instance 'Zero <= n where
  proofLTE = LTE1

instance (m <= n) => ('Succ m) <= ('Succ n) where
  proofLTE = LTE2 proofLTE

lteDict :: m :<=: n -> Dict (m <= n)
lteDict LTE1 = Dict
lteDict (LTE2 proof) = case lteDict proof of Dict -> Dict

upcastLt :: m :<: n -> m :<=: n
upcastLt = \case
  LT1 -> LTE1
  LT2 proof -> LTE2 $ upcastLt proof

extendLt :: m :<: n1 -> n1 :<=: n2 -> m :<: n2
extendLt LT1 (LTE2 _) = LT1
extendLt (LT2 ltSubProof) (LTE2 lteSubProof) =
  LT2 $ extendLt ltSubProof lteSubProof
