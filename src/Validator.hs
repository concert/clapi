{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Validator where

import Control.Applicative ((<|>), (<*))
import Control.Error.Util (note)
import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)
import Data.Scientific (toRealFloat)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.Regex.PCRE ((=~~))
import Text.Printf (printf, PrintfArg)

import qualified Data.Attoparsec.Text as Dat

import Path.Parsing (nameP)
import Types (ClapiValue(..), Time(..), Clapiable, fromClapiValue)
import Tree (CanFail)

type Validator = ClapiValue -> CanFail ()
success = Right ()

maybeP :: Dat.Parser a -> Dat.Parser (Maybe a)
maybeP p = (Just <$> p) <|> pure Nothing

fromText :: Text.Text -> CanFail Validator
fromText = Dat.parseOnly (mainParser <* Dat.endOfInput)
  where
    unwrapMaybe (Just foo) = foo -- Yuck, get rid of this!
    mainParser = do
        name <- Dat.choice $ fmap Dat.string $ Map.keys argsParserMap
        argValidator <- unwrapMaybe $ Map.lookup name argsParserMap
        return argValidator
    bracket p = do
        Dat.char '['
        result <- p
        Dat.char ']'
        return result
    sep = Dat.char ':'
    argsParserMap :: Map.Map Text.Text (Dat.Parser Validator)
    argsParserMap = Map.fromList [
        ("bool", optionalEmpty boolValidator),
        ("time", optionalEmpty timeValidator),
        ("enum", mandatoryArgs1 getEnumValidator enumP),
        ("word32", optionalArgs2 getNumValidator word32P word32P),
        ("word64", optionalArgs2 getNumValidator word64P word64P),
        ("int32", optionalArgs2 getNumValidator int32P int32P),
        ("int64", optionalArgs2 getNumValidator int64P int64P),
        ("float", optionalArgs2 getNumValidator floatP floatP),
        ("double", optionalArgs2 getNumValidator doubleP doubleP),
        -- FIXME: avoiding square brackets won't do for regexs :-(
        ("string", optionalArgs1 getStringValidator (Dat.many' $ Dat.notChar ']')),
        ("list", mandatoryArgs1 getListValidator mainParser)
        ]
    nothing = pure ()
    optionalEmpty validator = bracket nothing <|> nothing >> pure validator
    optionalArgs1 f argP =
        Dat.option (f Nothing) (bracket g)
      where
        g = f <$> maybeP argP
    optionalArgs2 f argAP argBP =
        Dat.option (f Nothing Nothing) (bracket g)
      where
        g = f <$> (maybeP argAP) <*> (maybeP $ sep >> argBP)
    mandatoryArgs1 f p = f <$> bracket p
    enumP = Dat.sepBy nameP (Dat.char ',')
    word32P = Dat.decimal :: Dat.Parser Word32
    word64P = Dat.decimal :: Dat.Parser Word64
    int32P = Dat.signed Dat.decimal :: Dat.Parser Int32
    int64P = Dat.signed Dat.decimal :: Dat.Parser Int64
    floatP = toRealFloat <$> Dat.scientific :: Dat.Parser Float
    doubleP = toRealFloat <$> Dat.scientific :: Dat.Parser Double

validate :: [Validator] -> [ClapiValue] -> CanFail ()
validate vs cvs
  | length vs > length cvs = Left "Insufficient values"
  | length vs < length cvs = Left "Insufficient validators"
  | otherwise = softValidate vs cvs

-- validate where lengths of lists aren't important
softValidate :: [Validator] -> [ClapiValue] -> CanFail ()
softValidate vs cvs =  foldl (>>) (Right ()) $ zipWith ($) vs cvs

boolValidator :: Validator
boolValidator (CBool _) = success
boolValidator _ = Left "Bad type"  -- FIXME: should say which!

timeValidator :: Validator
timeValidator (CTime (Time _ _)) = success
timeValidator _ = Left "Bad type"  -- FIXME: should say which!

getNumValidator :: forall a. (Clapiable a, Ord a, PrintfArg a) =>
    Maybe a -> Maybe a -> Validator
getNumValidator min max =
    \cv -> checkType cv >>= bound min max
  where
    -- FIXME: should say which type!
    checkType cv = note "Bad type" (fromClapiValue cv :: Maybe a)
    bound :: (Ord a) => Maybe a -> Maybe a -> a -> CanFail ()
    bound Nothing Nothing v = success
    bound (Just min) Nothing v
      | v >= min = success
      | otherwise = Left $ printf "%v is not >= %v" v min
    bound Nothing (Just max) v
      | v <= max = success
      | otherwise = Left $ printf "%v is not <= %v" v max
    bound (Just min) (Just max) v
      | v >= min && v <= max = success
      | otherwise = Left $ printf "%v not between %v and %v" v min max

getEnumValidator :: [String] -> Validator
getEnumValidator names (CEnum x)
  | x <= max = success
  | otherwise = Left $ printf "Enum value not <= %v" (max)
  where
    max = fromIntegral $ length names - 1
getEnumValidator _ _ = Left "Bad type"  -- FIXME: should say which!

getStringValidator :: Maybe String -> Validator
getStringValidator Nothing (CString t) = success
getStringValidator (Just pattern) (CString t) =
    note errStr ((Text.unpack t) =~~ pattern :: Maybe ())
  where
    errStr = printf "did not match '%s'" pattern
getStringValidator _ _ = Left "Bad type"  -- FIXME: should say which!

getListValidator :: Validator -> Validator
getListValidator itemValidator = listValidator
  where
    listValidator (CList xs) = softValidate (repeat itemValidator) xs
    listValidator _ = Left "Bad type"  -- FIXME: should say which!
