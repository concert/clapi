{-# LANGUAGE
   DataKinds
 , KindSignatures
 , GADTs
#-}

module Clapi.Types.TypeName where

import Clapi.Types.Path (Seg)

data TypeNamespace = TnTree | TnValue deriving (Eq, Show, Bounded, Enum)

newtype RawTypeName = RawTypeName (Seg, Seg)

data TypeName :: TypeNamespace -> * where
    TreeTypeName :: Seg -> Seg -> TypeName TnTree
    ValueTypeName :: Seg -> Seg -> TypeName TnValue

toRawTypeName :: TypeName a -> (RawTypeName, TypeNamespace)
toRawTypeName (TreeTypeName ns n) = (RawTypeName (ns, n), TnTree)
toRawTypeName (ValueTypeName ns n) = (RawTypeName (ns, n), TnValue)

data AnyTypeName
  = AtnTree (TypeName TnTree)
  | AtnValue (TypeName TnValue)

fromRawTypeName :: TypeNamespace -> RawTypeName -> AnyTypeName
fromRawTypeName tns (RawTypeName (ns, n)) = case tns of
    TnTree -> AtnTree $ TreeTypeName ns n
    TnValue -> AtnValue $ ValueTypeName ns n
