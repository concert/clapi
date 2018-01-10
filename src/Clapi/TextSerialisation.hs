{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Clapi.TextSerialisation where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Dat
import Data.Monoid ((<>))
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as Text

import Clapi.Types.Tree
  ( Bounds(..), TreeConcreteTypeName(..), TreeContainerTypeName(..)
  , TreeConcreteType(..), TreeContainerType(..), TreeType(..))
import Clapi.Path (pathP, segP)

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

contTNameParser :: Parser TreeContainerTypeName
contTNameParser = Dat.choice $
  fmap (\ctn -> Dat.string (contTNameToText ctn) >> return ctn) [minBound..]


argsOpen, argsClose, boundsSep, listSep :: Char
argsOpen = '['
argsClose = ']'
boundsSep = ':'
listSep = ','

boundsToText :: (Show a) => Bounds a -> Text
boundsToText bounds = Text.pack $ case bounds of
  Bounds Nothing Nothing -> ""
  Bounds (Just bmin) Nothing -> show bmin ++ [boundsSep]
  Bounds Nothing (Just bmax) -> boundsSep : show bmax
  Bounds (Just bmin) (Just bmax) -> show bmin ++ [boundsSep] ++ show bmax

maybeP :: Parser a -> Parser (Maybe a)
maybeP p = (Just <$> p) <|> return Nothing

spaces :: Parser ()
spaces = void $ Dat.many' $ Dat.char ' '

withSpaces :: Parser a -> Parser a
withSpaces p = spaces >> p <* spaces

sep'd :: Char -> Parser ()
sep'd c = void $ withSpaces $ Dat.char c

boundsParser :: Parser a -> Parser (Bounds a)
boundsParser p = do
  a <- maybeP p
  sep'd boundsSep
  b <- maybeP p
  return $ Bounds a b

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
    bbp = optionalArgs (Bounds Nothing Nothing) . boundsParser
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
      TcnRef -> TcRef <$> pathP
      TcnValidatorDesc -> return TcValidatorDesc

contTParser :: Parser TreeContainerType
contTParser = contTNameParser >>= getParser
  where
    getParser tcn = bracketed $ case tcn of
      TcnList -> TcList <$> ttParser
      TcnSet -> TcSet <$> ttParser
      TcnOrdSet -> TcOrdSet <$> ttParser

ttParser :: Parser TreeType
ttParser = (TtConc <$> concTParser) <|> (TtCont <$> contTParser)
