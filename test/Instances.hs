{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , StandaloneDeriving
#-}

module Instances where

import Data.String (IsString(..))

import Clapi.PerClientProto (ServerEvent(..))
import Clapi.Types
import Clapi.Types.SequenceOps (SequenceOp(..))


deriving instance (Ord ident, Ord a) => Ord (ServerEvent ident a)

deriving instance Ord Attributee
deriving instance IsString Attributee

deriving instance Ord PostDefinition
deriving instance Ord Definition
deriving instance Ord ArrayDefinition
deriving instance Ord StructDefinition
deriving instance Ord TupleDefinition
deriving instance Ord Editable

deriving instance Ord FrDigest
deriving instance Ord FrcRootDigest
deriving instance Ord FrcSubDigest
deriving instance Ord FrcUpdateDigest
deriving instance Ord FrpDigest
deriving instance Ord FrpErrorDigest

deriving instance Ord CreateOp
deriving instance Ord DataChange
deriving instance Ord a => Ord (DefOp a)
deriving instance Ord TimeSeriesDataOp

deriving instance Ord a => Ord (SequenceOp a)
