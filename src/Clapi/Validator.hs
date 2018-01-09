{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Clapi.Validator where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import qualified Data.Text as T
import Control.Applicative ((<|>), (<*))
import Control.Error.Util (note)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Word (Word32, Word64, Word8)
import Data.Int (Int32, Int64)
import Data.Scientific (toRealFloat)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Regex.PCRE ((=~~))
import Text.Printf (printf, PrintfArg)

import qualified Data.Attoparsec.Text as Dat

import Clapi.Util (ensureUnique)
import Clapi.Path (Path, isChildOf, segP, pathP, toText, unSeg)
import Clapi.Types (
    CanFail, WireValue(..), Time(..), Wireable, castWireValue)
import Clapi.Serialisation
  ( WireType(..), WireConcreteType(..), WireContainerType(..)
  , withWireTypeProxy)

type NodePath = Path
type TypePath = Path

data VType =
    VBool | VTime | VEnum | VWord32 | VWord64 | VInt32 | VInt64 |
    VFloat | VDouble | VString | VRef | VValidator | VList VType | VSet VType
  deriving (Eq, Show, Ord)

vTypeToWireType :: VType -> WireType
vTypeToWireType vt = case vt of
  VBool -> WtConc WcWord8
  VTime -> WtConc WcTime
  VEnum -> WtConc WcWord8
  VWord32 -> WtConc WcWord32
  VWord64 -> WtConc WcWord64
  VInt32 -> WtConc WcInt32
  VInt64 -> WtConc WcInt64
  VFloat -> WtConc WcFloat
  VDouble -> WtConc WcDouble
  VString -> WtConc WcString
  VRef -> WtConc WcString
  VValidator -> WtConc WcString
  VList vt' -> WtCont WcList $ vTypeToWireType vt'
  VSet vt' -> WtCont WcList $ vTypeToWireType vt'

type Validate =
    (NodePath -> CanFail TypePath) -> WireValue -> CanFail [NodePath]

data Validator = Validator {
    vDesc :: Text.Text,
    vValidate :: Validate,
    vType :: VType}

instance Show Validator where
  show = Text.unpack . vDesc

instance Eq Validator where
  v1 == v2 = vDesc v1 == vDesc v2

success :: (Monad m, Monoid a) => m a
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
        _ <- Dat.char '['
        result <- p
        _ <- Dat.char ']'
        return result
    sep = Dat.char ':'
    argsParserMap :: Map.Map Text.Text (Dat.Parser Validator)
    argsParserMap = Map.fromList [
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
    enumP = Dat.sepBy (T.unpack . unSeg <$> segP) (Dat.char ',')
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
    (NodePath -> CanFail TypePath) -> [Validator] -> [WireValue] ->
    CanFail [NodePath]
validate getTypePath vs cvs
  | length vs > length cvs = Left "Too few values"
  | length vs < length cvs = Left "Too many values"
  | otherwise = softValidate getTypePath vs cvs

toMonadFail :: MonadFail m => CanFail a -> m a
toMonadFail = either fail return

-- validate where lengths of lists aren't important
softValidate ::
    MonadFail m =>
    (NodePath -> CanFail TypePath) -> [Validator] -> [WireValue] -> m [NodePath]
softValidate getTypePath vs cvs =
    toMonadFail $
    fmap mconcat . sequence $
    zipWith ($) [(vValidate v) getTypePath | v <- vs] cvs

timeValidator :: Text.Text -> Validator
timeValidator t = Validator t doValidate VTime
  where
    doValidate _ wv = (castWireValue wv :: Either String Time) >> success

getNumValidator :: forall a. (Wireable a, Ord a, PrintfArg a) =>
    Text.Text -> VType -> Maybe a -> Maybe a -> Validator
getNumValidator t vtype mMin mMax =
     Validator t (\_ wv -> castWireValue @a wv >>= bound mMin mMax) vtype
  where
    bound :: (Ord a) => Maybe a -> Maybe a -> a -> CanFail [NodePath]
    bound Nothing Nothing _ = success
    bound (Just theMin) Nothing v
      | v >= theMin = success
      | otherwise = Left $ printf "%v is not >= %v" v theMin
    bound Nothing (Just theMax) v
      | v <= theMax = success
      | otherwise = Left $ printf "%v is not <= %v" v theMax
    bound (Just theMin) (Just theMax) v
      | v >= theMin && v <= theMax = success
      | otherwise = Left $ printf "%v not between %v and %v" v theMin theMax

getEnumValidator :: Text.Text -> [String] -> Validator
getEnumValidator t names = Validator t doValidate VEnum
  where
    theMax :: Word8
    theMax = fromIntegral $ length names - 1
    doValidate :: a -> WireValue -> CanFail [NodePath]
    doValidate _ wv = do
        i <- castWireValue wv
        if i <= theMax
          then success
          else Left $ printf "Enum value not <= %v" (theMax)

getStringValidator :: Text.Text -> Maybe String -> Validator
getStringValidator desc p = Validator desc (doValidate p) VString
  where
    errStr = printf "did not match '%s'"
    doValidate Nothing _ wv = castWireValue @Text wv >> success
    doValidate (Just pat) _ wv = do
        t <- castWireValue wv
        const [] <$>
            note (errStr pat) ((Text.unpack t) =~~ pat :: Maybe ())

getRefValidator :: Text.Text -> Path -> Validator
getRefValidator t requiredTypePath = Validator t doValidate VRef
  where
    doValidate getTypePath wv =
      do
        ptxt <- castWireValue wv
        nodePath <- Dat.parseOnly pathP ptxt
        typePath <- getTypePath nodePath
        if typePath `isChildOf` requiredTypePath
        then return . pure $ nodePath
        else Left $ printf "%v is of type %v, rather than expected %v"
          (toString nodePath) (toString typePath) (toString requiredTypePath)
    toString = T.unpack . toText


validatorValidator :: Text.Text -> Validator
validatorValidator t = Validator t doValidate VValidator
  where
    doValidate _ wv = castWireValue @Text wv >>= fromText >> success


getListValidator :: Text.Text -> Validator -> Validator
getListValidator t itemValidator =
    Validator t listValidator (VList $ vType itemValidator)
  where
    listValidator getType wv =
        withWireTypeProxy validateItems (vTypeToWireType $ vType itemValidator)
      where
        validateItems :: forall a. Wireable a => Proxy a -> CanFail [NodePath]
        validateItems _ =
          castWireValue @[a] wv >>=
          softValidate getType (repeat itemValidator) . fmap WireValue

showJoin :: (Show a, Foldable t) => String -> t a -> String
showJoin sep as = intercalate sep $ fmap show (toList as)

getSetValidator :: Text.Text -> Validator -> Validator
getSetValidator t itemValidator =
    Validator t setValidator (VSet $ vType itemValidator)
  where
    setValidator getType wv =
        withWireTypeProxy validateItems (vTypeToWireType $ vType itemValidator)
      where
        validateItems :: forall a. Wireable a => Proxy a -> CanFail [NodePath]
        validateItems _ =
          castWireValue @[a] wv >>=
          ensureUnique "items" >>=
          softValidate getType (repeat itemValidator) . fmap WireValue
