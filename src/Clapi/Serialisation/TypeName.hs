module Clapi.Serialisation.TypeName where

import Clapi.Serialisation.Base (Encodable(..))
import Clapi.Types.TypeName (TypeNameParseable, TypeName, typeNameToText, typeNameFromText)

instance TypeNameParseable a => Encodable (TypeName a) where
  builder = builder . typeNameToText
  parser = parser >>= typeNameFromText
