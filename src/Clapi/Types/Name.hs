{-# LANGUAGE
    DataKinds
  , DeriveLift
  , KindSignatures
#-}

module Clapi.Types.Name
  ( NameRole(..)
  , Name, mkName, unName, nameP, castName
  , DataName, DefName, PostDefName, Namespace, PostArgName, Placeholder
  , TupMemberName
  ) where

import Control.Monad.Fail (MonadFail)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as DAT
import Data.Char (isDigit, isLetter)
import Data.Text (Text)
import qualified Data.Text as Text
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (Lift)



data NameRole
  = ForData
  | ForTyDef
  | ForPostTyDef
  | ForNamespace
  | ForPostArg
  | ForPlaceholder
  | ForTupMember

newtype Name (nr :: NameRole) = Name {unName :: Text} deriving (Eq, Ord, Lift)

type DataName = Name 'ForData
type DefName = Name 'ForTyDef
type PostDefName = Name 'ForPostTyDef
type Namespace = Name 'ForNamespace
type PostArgName = Name 'ForPostArg
type Placeholder = Name 'ForPlaceholder
type TupMemberName = Name 'ForTupMember

instance Show (Name nr) where
    show = Text.unpack . unName

castName :: Name nr1 -> Name nr2
castName (Name n) = Name n

isValidNameChar :: Char -> Bool
isValidNameChar c = isLetter c || isDigit c || c == '_'

nameP :: Parser (Name nr)
nameP = Name <$> DAT.takeWhile1 isValidNameChar

mkName :: MonadFail m => Text -> m (Name nr)
mkName = either fail return . DAT.parseOnly (nameP <* DAT.endOfInput)

instance Semigroup (Name nr) where
  (Name t1) <> (Name t2) = Name (t1 <> Text.singleton '_' <> t2)
