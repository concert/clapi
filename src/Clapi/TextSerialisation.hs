{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Clapi.TextSerialisation where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Dat
import Data.Scientific (toRealFloat)

import Clapi.Types.Tree
  ( Bounds, bounds, unbounded, boundsMin, boundsMax, TreeConcreteTypeName(..),
  TreeContainerTypeName(..) , TreeConcreteType(..), TreeContainerType(..),
  TreeType(..), typeEnumOf)
import Clapi.Types.Path (segP, unSeg)
import qualified Clapi.Types.Path as Path

concTNameToText :: TreeConcreteTypeName -> Text
concTNameToText tc = case tc of
  TcnTime -> "time"
  TcnEnum -> "enum"
  TcnWord32 -> "word32"
  TcnWord64 -> "word64"
  TcnInt32 -> "int32"
  TcnInt64 -> "int64"
  TcnFloat -> "float"
  TcnDouble -> "double"
  TcnString -> "string"
  TcnRef -> "ref"
  TcnValidatorDesc -> "validator"

concTNameParser :: Parser TreeConcreteTypeName
concTNameParser = Dat.choice $
  fmap (\ctn -> Dat.string (concTNameToText ctn) >> return ctn) [minBound..]

contTNameToText :: TreeContainerTypeName -> Text
contTNameToText tc = case tc of
  TcnList -> "list"
  TcnSet -> "set"
  TcnOrdSet -> "ordSet"
  TcnMaybe -> "maybe"

contTNameParser :: Parser TreeContainerTypeName
contTNameParser = Dat.choice $
  fmap (\ctn -> Dat.string (contTNameToText ctn) >> return ctn) [minBound..]


argsOpen, argsClose, boundsSep, listSep :: Char
argsOpen = '['
argsClose = ']'
boundsSep = ':'
listSep = ','

boundsToText :: (Show a) => Bounds a -> Text
boundsToText b = case (boundsMin b, boundsMax b) of
    (Nothing, Nothing) -> ""
    (bmin, bmax) -> mShow bmin <> Text.singleton boundsSep <> mShow bmax
  where
    mShow = maybe "" (Text.pack . show)

maybeP :: Parser a -> Parser (Maybe a)
maybeP p = (Just <$> p) <|> return Nothing

spaces :: Parser ()
spaces = void $ Dat.many' $ Dat.char ' '

withSpaces :: Parser a -> Parser a
withSpaces p = spaces >> p <* spaces

sep'd :: Char -> Parser ()
sep'd c = void $ withSpaces $ Dat.char c

boundsParser :: Ord a => Parser a -> Parser (Bounds a)
boundsParser p = do
  a <- maybeP p
  sep'd boundsSep
  b <- maybeP p
  bounds a b

bracketed :: Parser a -> Parser a
bracketed p =
    Dat.char argsOpen
    >> (upToLastClose >>= either fail return . Dat.parseOnly p)
  where
    upToLastClose :: Parser Text
    upToLastClose = do
      t1 <- Dat.takeWhile (/= argsClose)
      t2 <- (Dat.char argsClose >> Dat.endOfInput >> return "")
            <|> ((<>) <$> (Text.singleton <$> Dat.char argsClose) <*> upToLastClose)
      return $ t1 <> t2


concTParser :: Parser TreeConcreteType
concTParser = concTNameParser >>= getParser
  where
    optionalArgs def p = bracketed p <|> return def
    bbp :: Ord a => Parser a -> Parser (Bounds a)
    bbp = optionalArgs unbounded . boundsParser
    getParser tcn = case tcn of
      TcnTime -> return TcTime
      TcnEnum -> TcEnum <$> bracketed (Dat.sepBy segP $ sep'd listSep)
      TcnWord32 -> TcWord32 <$> bbp Dat.decimal
      TcnWord64 -> TcWord64 <$> bbp Dat.decimal
      TcnInt32 -> TcInt32 <$> bbp (Dat.signed Dat.decimal)
      TcnInt64 -> TcInt64 <$> bbp (Dat.signed Dat.decimal)
      TcnFloat -> TcFloat <$> bbp (toRealFloat <$> Dat.scientific)
      TcnDouble -> TcDouble <$> bbp (toRealFloat <$> Dat.scientific)
      TcnString -> TcString <$> optionalArgs "" (Dat.takeWhile $ const True)
      TcnRef -> TcRef <$> bracketed Path.typeNameP
      TcnValidatorDesc -> return TcValidatorDesc

contTParser :: Parser TreeContainerType
contTParser = contTNameParser >>= getParser
  where
    getParser tcn = bracketed $ case tcn of
      TcnList -> TcList <$> ttParser
      TcnSet -> TcSet <$> ttParser
      TcnOrdSet -> TcOrdSet <$> ttParser
      TcnMaybe -> TcMaybe <$> ttParser

ttParser :: Parser TreeType
ttParser = (TtConc <$> concTParser) <|> (TtCont <$> contTParser)

ttFromText :: MonadFail m => Text -> m TreeType
ttFromText = either fail return . Dat.parseOnly (ttParser <* Dat.endOfInput)

bracketText :: Text -> Text
bracketText t = Text.singleton argsOpen <> t <> Text.singleton argsClose

concTToText :: TreeConcreteType -> Text
concTToText tct = (concTNameToText $ typeEnumOf tct) <> args
  where
    bracketContent = case tct of
        TcTime -> ""
        TcEnum segs -> Text.intercalate (Text.singleton listSep) (fmap unSeg segs)
        TcWord32 bs -> boundsToText bs
        TcWord64 bs -> boundsToText bs
        TcInt32 bs -> boundsToText bs
        TcInt64 bs -> boundsToText bs
        TcFloat bs -> boundsToText bs
        TcDouble bs -> boundsToText bs
        TcString s -> s
        TcRef tn -> Path.typeNameToText tn
        TcValidatorDesc -> ""
    args = case bracketContent of
        "" -> ""
        s -> bracketText s

contTToText :: TreeContainerType -> Text
contTToText tct = (contTNameToText $ typeEnumOf $ tct) <> bracketText (ttToText $ contTContainedType tct)

ttToText :: TreeType -> Text
ttToText tt = case tt of
    TtConc tct -> concTToText tct
    TtCont tct -> contTToText tct
