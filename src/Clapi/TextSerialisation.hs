{-# LANGUAGE OverloadedStrings #-}
module Clapi.TextSerialisation where
import Data.Monoid
import Data.Text (Text, unpack)
import Control.Applicative ((<|>))

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Char.Utf8 (fromString, fromChar, fromShow)

import Data.Attoparsec.Text (
    Parser, char, decimal, takeTill, many1, IResult(..), satisfy, inClass,
    parse, (<?>))

import qualified Clapi.Serialisation as Wire
import Clapi.Types (Time(..), ClapiValue(..), Message(..), Interpolation(..))
import Clapi.Path (Path)

typeTag :: ClapiValue -> Char
typeTag (ClBool _) = 'B'
typeTag v = Wire.typeTag v

cvBuilder :: ClapiValue -> Builder
cvBuilder (ClBool True) = fromChar 'T'
cvBuilder (ClBool False) = fromChar 'F'
cvBuilder (ClInt32 i) = fromShow i
cvBuilder (ClString s) = fromShow s

msgBuilder :: Message -> Builder
msgBuilder msg = case msg of
    (MsgSet {}) -> valB 's' valSubs msg
    (MsgAdd {}) -> valB 'a' valSubs msg
    (MsgRemove {}) -> valB 'r' noSubs msg
  where
    tb (Time s f) = (fromShow s) <> (fromChar ':') <> (fromShow f)
    ab ma = case ma of
        (Just  a) -> fromShow a
        Nothing -> fromString "\"\""
    tab subs msg = spJoin $
        [tb $ msgTime msg] ++ (subs msg) ++ [ab $ msgAttributee msg]
    spJoin bs = mconcat $ map (fromChar ' ' <>) bs
    vb vs = map cvBuilder vs
    ib i = case i of
        IConstant -> fromChar 'C'
        ILinear -> fromChar 'L'
    valSubs msg = (vb $ msgArgs' msg) ++ [ib $ msgInterpolation msg]
    noSubs _ = []
    valB methodChar subs msg = fromChar methodChar <> tab subs msg

encode :: [Message] -> Builder
encode msgs = header <> bodyBuilder <> fromString "\nend"
  where
    -- It is invalid for a time series to be empty, so this use of head is
    -- kinda fine, but the errors will suck:
    header = fromString $ map typeTag $ msgArgs' $ head $ msgs
    bodyBuilder = mconcat $ map (\tp -> fromChar '\n' <> msgBuilder tp) msgs

quotedString :: Parser Text
-- FIXME: escaped quotes unsupported
quotedString = do
    char '"'
    s <- takeTill (== '"')
    char '"'
    return s

cvParser :: Char -> Parser ClapiValue
cvParser 'B' = ((wordMatch 'T' True) <|> (wordMatch 'F' False)) <?> "ClBool"
  where
    wordMatch c v = char c >> return (ClBool v)
cvParser 'i' = (decimal >>= return . ClInt32) <?> "ClInt32"
cvParser 's' = (ClString <$> quotedString) <?> "ClString"

charIn :: String -> String -> Parser Char
charIn cls msg = satisfy (inClass cls) <?> msg

getTupleParser :: Parser (Parser [ClapiValue])
-- FIXME: type tag characters here must line up with above (and would be nice
-- to be like those on the wire too)
getTupleParser = sequence <$> many1 (
    charIn "Bis" "type" >>= \c -> return (char ' ' >> cvParser c))

timeParser :: Parser Time
timeParser = do
    s <- decimal
    char ':'
    f <- decimal
    return $ Time s f

attributeeParser :: Parser (Maybe String)
attributeeParser = nothingEmpty <$> (char ' ' >> quotedString)
  where
    nothingEmpty t = case unpack t of
        "" -> Nothing
        s -> Just s

interpolationParser :: Parser Interpolation
interpolationParser =
    char ' ' >> (charIn "CL" "interpolation") >>= return . asInterpolation
  where
    asInterpolation 'C' = IConstant
    asInterpolation 'L' = ILinear

msgParser :: Path -> Parser [ClapiValue] -> Parser Message
msgParser path valParser = do
    m <- charIn "asr" "message method"
    char ' '
    getSub m
  where
    getSub 'a' = do
        t <- timeParser
        v <- valParser
        i <- interpolationParser
        a <- attributeeParser
        return $ MsgAdd path t v i a Nothing
    -- FIXME: just like above
    getSub 's' = do
        t <- timeParser
        v <- valParser
        i <- interpolationParser
        a <- attributeeParser
        return $ MsgSet path t v i a Nothing
    getSub 'r' = do
        t <- timeParser
        a <- attributeeParser
        return $ MsgRemove path t a Nothing

decode :: Path -> Text -> Either String [Message]
decode path txt = case parse getTupleParser txt of
    Fail _ ctxs msg -> Left msg
    Partial _ -> Left "Cannot decode empty"
    Done remaining p -> case parse (many1 $ innerParser p) remaining of
        Fail _ ctxs msg -> Left $ (show ctxs) ++ " - " ++ msg
        Partial _ -> Left "Unexpected EOI (we need an end marker)"
        -- FIXME: nothing should be left over?
        Done _ msgs -> Right msgs
  where
    innerParser tupleParser = char '\n' >> msgParser path tupleParser
