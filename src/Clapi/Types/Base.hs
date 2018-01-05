module Clapi.Types.Base
  ( Time(..)
  ) where

import Data.Word

data Time = Time Word64 Word32 deriving (Eq, Show, Ord, Bounded)
