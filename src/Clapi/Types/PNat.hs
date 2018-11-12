{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , LambdaCase
  , RankNTypes
  , TypeFamilies
  , TypeOperators
  , StandaloneDeriving
#-}

module Clapi.Types.PNat where

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

-- | Type-level less-than function on Peano numbers
type family (m :: PNat) :< (n :: PNat) :: Bool where
  n :< 'Zero = 'False
  'Zero :< 'Succ n = 'True
  'Succ m :< 'Succ n = m :< n

-- | Perhaps produce a proof that m < n
(%<) :: SPNat m -> SPNat n -> Maybe (m :< n :~: 'True)
_ %< SPZero = Nothing
SPZero %< (SPSucc _) = Just Refl
(SPSucc m) %< (SPSucc n) = m %< n
