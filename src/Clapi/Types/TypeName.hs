{-# LANGUAGE
   DataKinds
 , KindSignatures
 , GADTs
 , OverloadedStrings
 , ScopedTypeVariables
 , TypeApplications
 , LambdaCase
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

data TypeNamespace = TnTree | TnValue deriving (Eq, Show, Bounded, Enum)

data TypeName :: TypeNamespace -> * where
    TreeTypeName :: Seg -> Seg -> TypeName TnTree
    ValueTypeName :: Seg -> Seg -> TypeName TnValue

qualSepChar :: Char
qualSepChar = ':'

typeNameToText :: TypeName a -> Text
typeNameToText tn = case tn of
    TreeTypeName ns s -> t ns s
    ValueTypeName ns s -> t ns s
  where
    t ns s = unSeg ns <> Text.singleton qualSepChar <> unSeg s

instance Show (TypeName a) where
  show tn = Text.unpack (typeNameToText tn)

getTypeNameP :: (Seg -> Seg -> TypeName a) -> Parser (TypeName a)
getTypeNameP con = con <$> segP <*> (DAT.char qualSepChar >> segP)

class TypeNameParseable a where
    typeNameP :: Parser (TypeName a)

instance TypeNameParseable TnTree where
    typeNameP = getTypeNameP TreeTypeName

instance TypeNameParseable TnValue where
    typeNameP = getTypeNameP ValueTypeName

typeNameFromText :: TypeNameParseable a => MonadFail m => Text -> m (TypeName a)
typeNameFromText =
    either fail return . DAT.parseOnly (typeNameP <* DAT.endOfInput)

data ChildTypeName a
  = CtnCont (TypeName a)
  | CtnConc (TypeName TnValue)

childTypeNameToText :: ChildTypeName a -> Text
childTypeNameToText stn = case stn of
    CtnCont tn -> Text.singleton 'C' <> typeNameToText tn
    CtnConc tn -> Text.singleton 'V' <> typeNameToText tn

childTypeNameP :: forall a. TypeNameParseable a => Parser (ChildTypeName a)
childTypeNameP = DAT.anyChar >>= \case
    'C' -> CtnCont <$> typeNameP @a
    'V' -> CtnConc <$> typeNameP @TnValue
