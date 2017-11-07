{-# LANGUAGE OverloadedStrings #-}
module Clapi.TextSerialisation where
import Data.Monoid
import Data.Text (Text, unpack, empty)
import Control.Applicative ((<|>))

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Char.Utf8 (fromString, fromChar, fromShow)

import Data.Attoparsec.Text (
    Parser, char, decimal, takeTill, many1, IResult(..), satisfy, inClass,
    parse, (<?>), endOfInput)

import qualified Clapi.Serialisation as Wire
import Clapi.Types (
    Time(..), ClapiTypeEnum(..), ClapiValue(..), Message(..), Interpolation(..))
import Clapi.Path (Path)

cvBuilder :: ClapiValue -> Builder
cvBuilder (ClInt32 i) = fromShow i
cvBuilder (ClString s) = fromShow s
-- I think this is the best you can do without path specific type info:
cvBuilder (ClEnum i) = fromShow i

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
    spJoin bs = mconcat $ fmap (fromChar ' ' <>) bs
    vb vs = fmap cvBuilder vs
    ib i = case i of
        IConstant -> fromChar 'C'
        ILinear -> fromChar 'L'
    valSubs msg = (vb $ msgArgs' msg) ++ [ib $ msgInterpolation msg]
    noSubs _ = []
    valB methodChar subs msg = fromChar methodChar <> tab subs msg

headerBuilder :: Message -> Builder
headerBuilder = fromString . fmap Wire.valueTag . msgArgs'

-- Not sure how useful this is because how often do you know all the messages
-- up front?
encode :: [Message] -> Builder
encode msgs = header <> bodyBuilder
  where
    -- It is invalid for a time series to be empty, so this use of head is
    -- kinda fine, but the errors will suck:
    header = headerBuilder $ head $ msgs
    bodyBuilder = mconcat $ fmap (\tp -> fromChar '\n' <> msgBuilder tp) msgs

quotedString :: Parser Text
-- FIXME: escaped quotes unsupported
quotedString = do
    char '"'
    s <- takeTill (== '"')
    char '"'
    return s

cvParser :: ClapiTypeEnum -> Parser ClapiValue
cvParser ClTInt32 = (ClInt32 <$> decimal) <?> "ClInt32"
cvParser ClTString = (ClString <$> quotedString) <?> "ClString"
cvParser ClTEnum = (ClEnum <$> decimal) <?> "ClEnum"

charIn :: String -> String -> Parser Char
charIn cls msg = satisfy (inClass cls) <?> msg

getTupleParser :: Parser (Parser [ClapiValue])
getTupleParser = sequence <$> many1 (charIn Wire.typeTags "type" >>= mkParser)
  where
    mkParser c = do
      clType <- Wire.typeFromTag c
      return $ char ' ' >> cvParser clType

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
    Done remaining p -> handleResult $ parse ((many1 $ innerParser p) <* endOfInput) remaining
  where
    innerParser tupleParser = char '\n' >> msgParser path tupleParser
    handleResult r = case r of
        Fail _ ctxs msg -> Left $ (show ctxs) ++ " - " ++ msg
        Partial cont -> handleResult $ cont empty
        -- FIXME: nothing should be left over?
        Done _ msgs -> Right msgs
