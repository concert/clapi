{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Clapi.Validator where

import Control.Applicative ((<|>), (<*))
import Control.Error.Util (note)
import Control.Monad (join)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)
import Data.Scientific (toRealFloat)
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Text.Regex.PCRE ((=~~))
import Text.Printf (printf, PrintfArg)

import qualified Data.Attoparsec.Text as Dat

import Clapi.Util (duplicates)
import Clapi.Path (Path, isChildOf)
import Path.Parsing (nameP, pathP, toString)
import Clapi.Types (
    CanFail, ClapiValue(..), Time(..), Clapiable, fromClapiValue)

type NodePath = Path
type TypePath = Path

data VType =
    VBool | VTime | VEnum | VWord32 | VWord64 | VInt32 | VInt64 |
    VFloat | VDouble | VString | VRef | VValidator | VList VType | VSet VType
  deriving (Eq, Show, Ord)

type Validate =
    (NodePath -> CanFail TypePath) -> ClapiValue -> CanFail [NodePath]

data Validator = Validator {
    desc :: Text.Text,
    goValidate :: Validate,
    vType :: VType}

instance Show Validator where
  show = Text.unpack . desc

instance Eq Validator where
  v1 == v2 = desc v1 == desc v2

success = return mempty

maybeP :: Dat.Parser a -> Dat.Parser (Maybe a)
maybeP p = (Just <$> p) <|> pure Nothing

fromText :: Text.Text -> CanFail Validator
fromText t = Dat.parseOnly (mainParser <* Dat.endOfInput) t
  where
    mainParser = do
        name <- Dat.choice $ fmap Dat.string $ Map.keys argsParserMap
        argValidator <- fromJust $ Map.lookup name argsParserMap
        return argValidator
    bracket p = do
        Dat.char '['
        result <- p
        Dat.char ']'
        return result
    sep = Dat.char ':'
    argsParserMap :: Map.Map Text.Text (Dat.Parser Validator)
    argsParserMap = Map.fromList [
        ("bool", optionalEmpty (boolValidator t)),
        ("time", optionalEmpty (timeValidator t)),
        ("enum", mandatoryArgs1 (getEnumValidator t) enumP),
        ("word32", optionalArgs2 (getNumValidator t VWord32) word32P word32P),
        ("word64", optionalArgs2 (getNumValidator t VWord64) word64P word64P),
        ("int32", optionalArgs2 (getNumValidator t VInt32) int32P int32P),
        ("int64", optionalArgs2 (getNumValidator t VInt64) int64P int64P),
        ("float", optionalArgs2 (getNumValidator t VFloat) floatP floatP),
        ("double", optionalArgs2 (getNumValidator t VDouble) doubleP doubleP),
        -- FIXME: avoiding square brackets won't do for regexs :-(
        ("string", optionalArgs1 (getStringValidator t) (Dat.many' $ Dat.notChar ']')),
        ("ref", mandatoryArgs1 (getRefValidator t) pathP),
        ("validator", mandatoryEmpty (validatorValidator t)),
        ("list", mandatoryArgs1 (getListValidator t) mainParser),
        -- FIXME: ulist or some other name implying uniquenss + order
        ("set", mandatoryArgs1 (getSetValidator t) mainParser)
        ]
    nothing = pure ()
    optionalEmpty validator = bracket nothing <|> nothing >> pure validator
    mandatoryEmpty validator = nothing >> pure validator
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


enumDesc :: (Enum a, Bounded a, Show a) => a -> Text.Text
enumDesc enum = Text.toLower . Text.pack $ printf "enum[%v]" $
    intercalate "," $ fmap show [minBound..maxBound `asTypeOf` enum]

-- FIXME: could use strictzip :-)
validate ::
    (NodePath -> CanFail TypePath) -> [Validator] -> [ClapiValue] ->
    CanFail [NodePath]
validate getTypePath vs cvs
  | length vs > length cvs = Left "Too few values"
  | length vs < length cvs = Left "Too many values"
  | otherwise = softValidate getTypePath vs cvs

-- validate where lengths of lists aren't important
softValidate ::
    (NodePath -> CanFail TypePath) -> [Validator] -> [ClapiValue] ->
    CanFail [NodePath]
softValidate getTypePath vs cvs =
    fmap mconcat . sequence $
    zipWith ($) [(goValidate v) getTypePath | v <- vs] cvs

boolValidator :: Text.Text -> Validator
boolValidator t = Validator t doValidate VBool
  where
    doValidate _ (CBool _) = success
    doValidate _ _ = Left "Bad type"  -- FIXME: should say which!

timeValidator :: Text.Text -> Validator
timeValidator t = Validator t doValidate VTime
  where
    doValidate _ (CTime (Time _ _)) = success
    doValidate _ _ = Left "Bad type"  -- FIXME: should say which!

getNumValidator :: forall a. (Clapiable a, Ord a, PrintfArg a) =>
    Text.Text -> VType -> Maybe a -> Maybe a -> Validator
getNumValidator t vtype min max =
    Validator t (\_ cv -> checkType cv >>= bound min max) vtype
  where
    -- FIXME: should say which type!
    checkType cv = note "Bad type" (fromClapiValue cv :: Maybe a)
    bound :: (Ord a) => Maybe a -> Maybe a -> a -> CanFail [NodePath]
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

getEnumValidator :: Text.Text -> [String] -> Validator
getEnumValidator t names = Validator t doValidate VEnum
  where
    max = fromIntegral $ length names - 1
    doValidate _ (CEnum x)
      | x <= max = success
      | otherwise = Left $ printf "Enum value not <= %v" (max)
    doValidate _ _ = Left "Bad type"  -- FIXME: should say which!

getStringValidator :: Text.Text -> Maybe String -> Validator
getStringValidator desc p = Validator desc (doValidate p) VString
  where
    errStr = printf "did not match '%s'"
    doValidate Nothing _ (CString t) = success
    doValidate (Just pattern) _ (CString t) = const [] <$>
        note (errStr pattern) ((Text.unpack t) =~~ pattern :: Maybe ())
    doValidate _ _ _ = Left "Bad type"  -- FIXME: should say which!

getRefValidator :: Text.Text -> Path -> Validator
getRefValidator t requiredTypePath = Validator t doValidate VRef
  where
    doValidate getTypePath (CString x) =
      do
        nodePath <- Dat.parseOnly pathP x
        typePath <- getTypePath nodePath
        if typePath `isChildOf` requiredTypePath
        then return . pure $ nodePath
        else Left $ printf "%v is of type %v, rather than expected %v"
          (toString nodePath) (toString typePath) (toString requiredTypePath)
    doValidate _ _ = Left "Bad type"  -- FIXME: should say which!


validatorValidator :: Text.Text -> Validator
validatorValidator t = Validator t doValidate VValidator
  where
    doValidate _ (CString x) = fromText x >> success
    doValidate _ _ = Left "Bad type"  -- FIXME: should say which!


getListValidator :: Text.Text -> Validator -> Validator
getListValidator t itemValidator =
    Validator t listValidator (VList $ vType itemValidator)
  where
    listValidator getType (CList xs) = softValidate getType (repeat itemValidator) xs
    listValidator _ _ = Left "Bad type"  -- FIXME: should say which!

showJoin :: (Show a, Foldable t) => String -> t a -> String
showJoin sep as = intercalate sep $ fmap show (toList as)

getSetValidator :: Text.Text -> Validator -> Validator
getSetValidator t itemValidator =
    Validator t setValidator (VSet $ vType itemValidator)
  where
    listValidator = getListValidator "" itemValidator
    setValidator getType cv@(CList xs) =
        let dups = duplicates xs in
        if null dups then goValidate listValidator getType cv
        else Left $ printf "Duplicate elements %v" $ showJoin ", " dups
    setValidator _ _ = Left "Bad type"
