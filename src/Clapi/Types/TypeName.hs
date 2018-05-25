{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
   DataKinds
 , KindSignatures
 , GADTs
 , OverloadedStrings
 , ScopedTypeVariables
 , TypeApplications
 , LambdaCase
 , StandaloneDeriving
#-}

module Clapi.Types.TypeName where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import qualified Data.Attoparsec.Text as DAT
import Data.Attoparsec.Text (Parser)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)

import Clapi.Types.Path (Seg(unSeg), segP)

data TypeNamespace = TnTree | TnCreate | TnValue deriving (Eq, Ord, Show, Bounded, Enum)

data TypeName :: TypeNamespace -> * where
    TreeTypeName :: Seg -> Seg -> TypeName 'TnTree
    CreateTypeName :: Seg -> Seg -> TypeName 'TnCreate
    ValueTypeName :: Seg -> Seg -> TypeName 'TnValue
deriving instance Eq (TypeName a)
-- Doesn't derive nicely:
instance Ord (TypeName a) where
    compare (TreeTypeName ns0 n0) (TreeTypeName ns1 n1) = compare (ns0, n0) (ns1, n1)
    compare (CreateTypeName ns0 n0) (CreateTypeName ns1 n1) = compare (ns0, n0) (ns1, n1)
    compare (ValueTypeName ns0 n0) (ValueTypeName ns1 n1) = compare (ns0, n0) (ns1, n1)

qualSepChar :: Char
qualSepChar = ':'

tnNamespace :: TypeName a -> Seg
tnNamespace = fst . rawTypeName

rawTypeName :: TypeName a -> (Seg, Seg)
rawTypeName tn = case tn of
    TreeTypeName ns s -> (ns, s)
    CreateTypeName ns s -> (ns, s)
    ValueTypeName ns s -> (ns, s)

rawTypeNameToText :: (Seg, Seg) -> Text
rawTypeNameToText (ns, s) = unSeg ns <> Text.singleton qualSepChar <> unSeg s

typeNameToText :: TypeName a -> Text
typeNameToText = rawTypeNameToText . rawTypeName
  where

instance Show (TypeName a) where
  show tn = Text.unpack (typeNameToText tn)

data AnyTypeName = AnyTypeName TypeNamespace (Seg, Seg) deriving (Eq, Ord, Show)

getTypeNameP :: (Seg -> Seg -> TypeName a) -> Parser (TypeName a)
getTypeNameP con = con <$> segP <*> (DAT.char qualSepChar >> segP)

class TypeNameParseable a where
    typeNameCons :: Seg -> Seg -> TypeName a
    toAnyTypeName :: TypeName a -> AnyTypeName

instance TypeNameParseable 'TnTree where
    typeNameCons = TreeTypeName
    toAnyTypeName = AnyTypeName TnTree . rawTypeName

instance TypeNameParseable 'TnCreate where
    typeNameCons = CreateTypeName
    toAnyTypeName = AnyTypeName TnCreate . rawTypeName

instance TypeNameParseable 'TnValue where
    typeNameCons = ValueTypeName
    toAnyTypeName = AnyTypeName TnValue . rawTypeName

parseText :: MonadFail m => Parser a -> Text -> m a
parseText p = either fail return . DAT.parseOnly (p <* DAT.endOfInput)

typeNameFromText :: TypeNameParseable a => MonadFail m => Text -> m (TypeName a)
typeNameFromText = parseText $ getTypeNameP typeNameCons

data ChildTypeName a
  = CtnCont (TypeName a)
  | CtnConc (TypeName 'TnValue)
  deriving (Show, Eq, Ord)

ctnConc :: Seg -> Seg -> ChildTypeName a
ctnConc ns n = CtnConc $ ValueTypeName ns n

ctnCont :: TypeNameParseable a => Seg -> Seg -> ChildTypeName a
ctnCont ns n = CtnCont $ typeNameCons ns n 

rawChildTypeName :: ChildTypeName a -> (Seg, Seg)
rawChildTypeName ctn = case ctn of
    CtnCont tn -> rawTypeName tn
    CtnConc tn -> rawTypeName tn

ctnToAnyTypeName :: TypeNameParseable a => ChildTypeName a -> AnyTypeName
ctnToAnyTypeName ctn = case ctn of
    CtnCont tn -> toAnyTypeName tn
    CtnConc tn -> toAnyTypeName tn

ctnNamespace :: ChildTypeName a -> Seg
ctnNamespace = fst . rawChildTypeName

childTypeNameToText :: ChildTypeName a -> Text
childTypeNameToText stn = case stn of
    CtnCont tn -> Text.singleton 'C' <> typeNameToText tn
    CtnConc tn -> Text.singleton 'V' <> typeNameToText tn

childTypeNameP :: forall a. TypeNameParseable a => Parser (ChildTypeName a)
childTypeNameP = DAT.anyChar >>= \case
    'C' -> CtnCont <$> getTypeNameP (typeNameCons @a)
    'V' -> CtnConc <$> getTypeNameP (typeNameCons @'TnValue)
    _ -> fail "Not C or V"

childTypeNameFromText :: (MonadFail m, TypeNameParseable a) => Text -> m (ChildTypeName a)
childTypeNameFromText = parseText childTypeNameP

data ChildSeg (a :: TypeNamespace) where
    CsConc :: Seg -> ChildSeg 'TnValue
    CsCont :: Seg -> ChildSeg a

promoteChildSeg :: Seg -> ChildSeg a -> ChildTypeName a
promoteChildSeg ns cs = case cs of
    CsConc s -> CtnCont $ ValueTypeName ns s
    CsCont s -> CtnConc $ typeNameCons ns s

anyTypeNameToText :: AnyTypeName -> Text
anyTypeNameToText (AnyTypeName tns rtn) = case tns of
    TnTree -> Text.singleton 't' <> rawTypeNameToText rtn
    TnCreate -> Text.singleton 'c' <> rawTypeNameToText rtn
    TnValue -> Text.singleton 'v' <> rawTypeNameToText rtn

atnNamespace :: AnyTypeName -> Seg
atnNamespace (AnyTypeName _ (ns, _)) = ns

atnWithTn :: (TypeName 'TnTree -> r) -> (TypeName 'TnCreate -> r) -> (TypeName 'TnValue -> r) -> AnyTypeName -> r
atnWithTn t c v (AnyTypeName tns (ns, s)) = case tns of
    TnTree -> t $ TreeTypeName ns s
    TnCreate -> c $ CreateTypeName ns s
    TnValue -> v $ ValueTypeName ns s
