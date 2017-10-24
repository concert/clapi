module Clapi.TextSerialisation where
import Data.Monoid
import qualified Data.Map as Map
import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Char.Utf8 (fromString, fromChar)
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

encode :: Map.Map Time [ClapiValue] -> Builder
encode ts = header <> bodyBuilder
  where
    -- It is invalid for a time series to be empty, so this use of head is
    -- kinda fine, but the errors will suck:
    header = fromString $ map typeTag $ head $ Map.elems ts
    bodyBuilder = mconcat $ map (\tp -> fromChar '\n' <> tpBuilder tp) (Map.toList ts)
