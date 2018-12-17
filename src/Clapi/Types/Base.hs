{-# LANGUAGE
    DeriveLift
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , FunctionalDependencies
#-}

module Clapi.Types.Base
  ( Tag, unTag, mkTag
  , Time(..), TimeStamped(..)
  , Attributee(..)
  , InterpolationLimit(..), Interpolation(..)
  , InterpolationType(..), interpolationType
  , TypeEnumOf(..)
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Language.Haskell.TH.Lift (Lift)

import Data.Char (chr)
import Data.Text (Text)
import Data.Word

newtype Tag = Tag {unTag :: Word8} deriving (Eq, Ord, Enum, Num, Lift)
instance Bounded Tag where
  minBound = Tag 32
  maxBound = Tag 126
instance Show Tag where
  show (Tag w) = ['<', chr $ fromIntegral $ w, '>']

mkTag :: MonadFail m => Word8 -> m Tag
mkTag w | minBound <= w && w <= maxBound = return $ Tag w
        | otherwise = fail "Tag not printable!"


data Time = Time Word64 Word32 deriving (Eq, Show, Ord, Bounded)

newtype TimeStamped a = TimeStamped (Time, a) deriving (Show, Functor)

newtype Attributee = Attributee {unAttributee :: Text} deriving (Show, Eq)

data InterpolationLimit = ILUninterpolated | ILConstant | ILLinear | ILBezier
  deriving (Show, Eq, Ord, Enum, Bounded)
data Interpolation = IConstant | ILinear | IBezier Word32 Word32
  deriving (Show, Eq, Ord)
data InterpolationType = ItConstant | ItLinear | ItBezier
  deriving (Show, Eq, Ord, Enum, Bounded)

interpolationType :: Interpolation -> InterpolationType
interpolationType IConstant = ItConstant
interpolationType ILinear = ItLinear
interpolationType (IBezier _ _) = ItBezier


class (Bounded b, Enum b) => TypeEnumOf a b | a -> b where
  typeEnumOf :: a -> b
