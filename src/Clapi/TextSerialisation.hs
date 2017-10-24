module Clapi.TextSerialisation where
import Data.Monoid
import Data.Text (Text)
import Control.Applicative ((<|>))
import qualified Data.Map as Map

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Char.Utf8 (fromString, fromChar)

import Data.Attoparsec.Text (Parser, char, decimal, takeTill, many1, IResult(..), satisfy, inClass, parse, endOfInput)

import qualified Clapi.Serialisation as Wire
import Clapi.Types (Time(..), ClapiValue(..))

typeTag :: ClapiValue -> Char
typeTag (ClBool _) = 'B'
typeTag v = Wire.typeTag v

fss :: Show a => a -> Builder
fss a = fromString $ show a

cvBuilder :: ClapiValue -> Builder
cvBuilder (ClBool True) = fromChar 'T'
cvBuilder (ClBool False) = fromChar 'F'
cvBuilder (ClInt32 i) = fss i
cvBuilder (ClString s) = fss s

tpBuilder :: (Time, [ClapiValue]) -> Builder
tpBuilder ((Time s f), vs) = tb <> mconcat (map (\v -> (fromChar ' ') <> (cvBuilder v)) vs)
  where
    tb = (fss s) <> (fromChar ':') <> (fss f)

type TimeSeries = Map.Map Time [ClapiValue]

encode :: TimeSeries -> Builder
encode ts = header <> bodyBuilder
  where
    -- It is invalid for a time series to be empty, so this use of head is
    -- kinda fine, but the errors will suck:
    header = fromString $ map typeTag $ head $ Map.elems ts
    bodyBuilder = mconcat $ map (\tp -> fromChar '\n' <> tpBuilder tp) (Map.toList ts)

quotedString :: Parser Text
-- FIXME: escaped quotes unsupported
quotedString = char '"' >> takeTill (== '"')

cvParser :: Char -> Parser ClapiValue
cvParser 'B' = (wordMatch 'T' True) <|> (wordMatch 'F' False)
  where
    wordMatch c v = char c >> return (ClBool v)
cvParser 'i' = decimal >>= return . ClInt32
cvParser 's' = ClString <$> quotedString

getTupleParser :: Parser (Parser [ClapiValue])
getTupleParser = sequence <$> many1 (satisfy (inClass "Bis") >>= \c -> return (char ' ' >> cvParser c))

timeParser :: Parser Time
timeParser = do
    s <- decimal
    char ':'
    f <- decimal
    return $ Time s f

decode :: Text -> Either String TimeSeries
decode txt = case parse getTupleParser txt of
    Fail _ ctxs msg -> Left msg
    Partial _ -> Left "Cannot decode empty"
    Done remaining p -> case parse (many1 $ innerParser p) remaining of
        Fail _ _ msg -> Left msg
        Partial _ -> Left "Unexpected EOI"
        -- FIXME: nothing should be left over?
        Done _ tsp -> Right $ Map.fromList tsp
  where
    innerParser tupleParser = do
        char '\n'
        t <- timeParser
        vs <- tupleParser
        return (t, vs)
