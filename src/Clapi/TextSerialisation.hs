{-# LANGUAGE
    GADTs
  , OverloadedStrings
#-}

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

import Clapi.Types.Base (typeEnumOf)
import Clapi.Types.Tree
import Clapi.Types.Path (nameP, unName)
import qualified Clapi.Types.Path as Path
import qualified Clapi.Types.SymbolList as SL


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
bracketed p = do
  _ <- Dat.char argsOpen
  res <- p
  _ <- Dat.char argsClose
  return res

optionalBracket :: a -> Parser a -> Parser a
optionalBracket def p = do
  mOpen <- maybeP $ Dat.char argsOpen
  case mOpen of
    Nothing -> return def
    Just _ -> p <* Dat.char argsClose

bracketText :: Text -> Text
bracketText t = Text.singleton argsOpen <> t <> Text.singleton argsClose

bracketNotNull :: Text -> Text
bracketNotNull t = case t of
  "" -> ""
  _ -> bracketText t

ttToText :: TreeType a -> Text
ttToText tt = (ttNameToText $ typeEnumOf tt) <> bracketNotNull bracketContent
  where
    bracketContent = case tt of
      TtTime -> ""
      TtEnum sl -> Text.intercalate (Text.singleton listSep) $
        Text.pack <$> SL.toStrings sl
      TtWord32 b -> boundsToText b
      TtWord64 b -> boundsToText b
      TtInt32 b -> boundsToText b
      TtInt64 b -> boundsToText b
      TtFloat b -> boundsToText b
      TtDouble b -> boundsToText b
      TtString r -> r
      TtRef ts -> Path.unName ts
      TtList tt' -> ttToText tt'
      TtSet tt' -> ttToText tt'
      TtOrdSet tt' -> ttToText tt'
      TtMaybe tt' -> ttToText tt'
      TtPair tt1 tt2 -> ttToText tt1 <> Text.singleton listSep <> ttToText tt2

ttToText_ :: SomeTreeType -> Text
ttToText_ = withTreeType ttToText

ttParser' :: Parser SomeTreeType
ttParser' = ttNameParser >>= argsParser
  where
    bbp :: Ord a => Parser a -> Parser (Bounds a)
    bbp = optionalBracket unbounded . boundsParser
    regex =
      let
        f (esc, n) c = if esc then Just (False, n) else
          case () of
            _ | c == argsOpen -> Just (False, n + 1)
              | c == argsClose ->
                if n - 1 == 0 then Nothing else Just (False, n - 1)
              | c == '\\' -> Just (True, n)
              | otherwise -> Just (False, n)
      in
        Dat.scan (False, 1 :: Int) f
    argsParser ttn = case ttn of
      TtnTime -> return ttTime
      TtnEnum -> ttEnum <$> fmap (Text.unpack . unName)
        <$> optionalBracket [] (Dat.sepBy nameP $ sep'd listSep)
      TtnWord32 -> ttWord32 <$> bbp Dat.decimal
      TtnWord64 -> ttWord64 <$> bbp Dat.decimal
      TtnInt32 -> ttInt32 <$> bbp (Dat.signed Dat.decimal)
      TtnInt64 -> ttInt64 <$> bbp (Dat.signed Dat.decimal)
      TtnFloat -> ttFloat <$> bbp (toRealFloat <$> Dat.scientific)
      TtnDouble -> ttDouble <$> bbp (toRealFloat <$> Dat.scientific)
      TtnString -> ttString <$> optionalBracket "" regex
      TtnRef -> ttRef <$> bracketed Path.nameP
      TtnList -> bracketed $ ttList <$> ttParser'
      TtnSet -> bracketed $ ttSet <$> ttParser'
      TtnOrdSet -> bracketed $ ttOrdSet <$> ttParser'
      TtnMaybe -> bracketed $ ttMaybe <$> ttParser'
      TtnPair -> bracketed $ do
        tt1 <- ttParser'
        sep'd listSep
        tt2 <- ttParser'
        return $ ttPair tt1 tt2

ttFromText :: MonadFail m => Text -> m SomeTreeType
ttFromText = either fail return . Dat.parseOnly (ttParser' <* Dat.endOfInput)

ttNameToText :: TreeTypeName -> Text
ttNameToText ttn = case ttn of
  TtnTime -> "time"
  TtnEnum -> "enum"
  TtnWord32 -> "word32"
  TtnWord64 -> "word64"
  TtnInt32 -> "int32"
  TtnInt64 -> "int64"
  TtnFloat -> "float"
  TtnDouble -> "double"
  TtnString -> "string"
  TtnRef -> "ref"
  TtnList -> "list"
  TtnSet -> "set"
  TtnOrdSet -> "ordSet"
  TtnMaybe -> "maybe"
  TtnPair -> "pair"

ttNameParser :: Parser TreeTypeName
ttNameParser = Dat.choice $
  fmap (\ctn -> Dat.string (ttNameToText ctn) >> return ctn) [minBound..]
