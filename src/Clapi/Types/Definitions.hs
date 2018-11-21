{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , LambdaCase
  , StandaloneDeriving
#-}

module Clapi.Types.Definitions where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Tagged (Tagged)
import Data.Text (Text)

import Data.Maybe.Clapi (note)

import Clapi.Types.AssocList (AssocList, unAssocList)
import Clapi.Types.Base (InterpolationLimit(..))
import Clapi.Types.Path (Seg)
import Clapi.Types.Tree (SomeTreeType(..))

data Editable = Editable | ReadOnly deriving (Show, Eq, Enum, Bounded)

data MetaType = Tuple | Struct | Array deriving (Show, Eq, Enum, Bounded)

type DefName = Tagged SomeDefinition Seg
type PostDefName = Tagged PostDefinition Seg

data Definition (mt :: MetaType) where
  TupleDef ::
    { tupDefDoc :: Text
    -- FIXME: this should eventually boil down to a single TreeType (NB remove
    -- names too and just write more docstring) now that we have pairs:
    , tupDefTys :: AssocList Seg SomeTreeType
    , tupDefILimit :: InterpolationLimit
    } -> Definition 'Tuple
  StructDef ::
    { strDefDoc :: Text
    , strDefChildTys :: AssocList Seg (DefName, Editable)
    } -> Definition 'Struct
  ArrayDef ::
    { arrDefDoc :: Text
    , arrDefPostTy :: Maybe PostDefName
    , arrDefChildTy :: DefName
    , arrDefChildEditable :: Editable
    } -> Definition 'Array
deriving instance Show (Definition mt)

data SomeDefinition where
  SomeDefinition :: Definition mt -> SomeDefinition
deriving instance Show SomeDefinition

tupleDef
  :: Text -> AssocList Seg SomeTreeType -> InterpolationLimit -> SomeDefinition
tupleDef doc tys ilimit = SomeDefinition $ TupleDef doc tys ilimit

structDef :: Text -> AssocList Seg (DefName, Editable) -> SomeDefinition
structDef doc tyinfo = SomeDefinition $ StructDef doc tyinfo

arrayDef :: Text -> Maybe PostDefName -> DefName -> Editable -> SomeDefinition
arrayDef doc ptn tn ed = SomeDefinition $ ArrayDef doc ptn tn ed

metaType :: Definition mt -> MetaType
metaType = \case
  TupleDef {} -> Tuple
  StructDef {} -> Struct
  ArrayDef {} -> Array

data PostDefinition = PostDefinition
  { postDefDoc :: Text
  -- FIXME: We really need to stop treating single values as lists of types,
  -- which makes the "top level" special:
  , postDefArgs :: AssocList Seg [SomeTreeType]
  } deriving (Show)

