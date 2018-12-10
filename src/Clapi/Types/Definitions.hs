{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , LambdaCase
  , MultiParamTypeClasses
  , RankNTypes
  , StandaloneDeriving
#-}

module Clapi.Types.Definitions where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Tagged (Tagged)
import Data.Text (Text)

import Data.Maybe.Clapi (note)

import Clapi.Types.AssocList (AssocList, unAssocList)
import Clapi.Types.Base (InterpolationLimit(..), TypeEnumOf(..))
import Clapi.Types.Path (Seg)
import Clapi.Types.Tree (TreeType(..))

data Editability = Editable | ReadOnly deriving (Show, Eq, Enum, Bounded)

data MetaType = Tuple | Struct | Array deriving (Show, Eq, Ord, Enum, Bounded)

type DefName = Tagged SomeDefinition Seg
type PostDefName = Tagged PostDefinition Seg

data PostDefinition = PostDefinition
  { postDefDoc :: Text
  -- FIXME: We really need to stop treating single values as lists of types,
  -- which makes the "top level" special:
  , postDefArgs :: AssocList Seg [TreeType]
  } deriving (Show, Eq)

data Definition (mt :: MetaType) where
  TupleDef ::
    { tupDefDoc :: Text
  -- FIXME: this should eventually boil down to a single TreeType (NB remove
  -- names too and just write more docstring) now that we have pairs:
    , tupDefTys :: AssocList Seg TreeType
    , tupDefILimit :: InterpolationLimit
    } -> Definition 'Tuple
  StructDef ::
    { strDefDoc :: Text
    , strDefChildTys :: AssocList Seg (DefName, Editability)
    } -> Definition 'Struct
  ArrayDef ::
    { arrDefDoc :: Text
    , arrDefPostTy :: Maybe PostDefName
    , arrDefChildTy :: DefName
    , arrDefChildEd :: Editability
    } -> Definition 'Array
deriving instance Show (Definition mt)

data SomeDefinition where
  SomeDefinition :: Definition mt -> SomeDefinition
deriving instance Show SomeDefinition

withDefinition :: (forall mt. Definition mt -> r) -> SomeDefinition -> r
withDefinition f (SomeDefinition d) = f d

tupleDef
  :: Text -> AssocList Seg TreeType -> InterpolationLimit -> SomeDefinition
tupleDef doc tys ilimit = SomeDefinition $ TupleDef doc tys ilimit

structDef :: Text -> AssocList Seg (DefName, Editability) -> SomeDefinition
structDef doc tyinfo = SomeDefinition $ StructDef doc tyinfo

arrayDef
  :: Text -> Maybe PostDefName -> DefName -> Editability -> SomeDefinition
arrayDef doc ptn tn ed = SomeDefinition $ ArrayDef doc ptn tn ed

instance TypeEnumOf (Definition mt) MetaType where
  typeEnumOf = \case
    TupleDef {} -> Tuple
    StructDef {} -> Struct
    ArrayDef {} -> Array

instance TypeEnumOf SomeDefinition MetaType where
  typeEnumOf = withDefinition typeEnumOf

childTypeFor :: Seg -> Definition mt -> Maybe DefName
childTypeFor seg = \case
  TupleDef {} -> Nothing
  StructDef { strDefChildTys = tyinfo } ->
    fst <$> lookup seg (unAssocList tyinfo)
  ArrayDef { arrDefChildTy = tn } -> Just tn

childEditableFor :: MonadFail m => Seg -> Definition mt -> m Editability
childEditableFor seg = \case
  TupleDef {} -> fail "Tuples have not children"
  StructDef { strDefChildTys = tyinfo } -> note "No such child" $
    snd <$> lookup seg (unAssocList tyinfo)
  ArrayDef { arrDefChildEd = ed } -> return ed
