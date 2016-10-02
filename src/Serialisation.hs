module Serialisation
    (
      ClapiValue(..),
      encodeList
    ) where

import Data.Monoid ((<>), Sum)
import qualified Data.ByteString as B
import Data.Int (Int32, Int64)
import Blaze.ByteString.Builder (
  Builder, toByteString, fromInt32be, fromInt64be)
import Data.ByteString.Builder(floatBE, doubleBE)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString, fromChar)

prefixLength :: Builder -> Builder
prefixLength b = byteSize bs <> fromByteString bs where
    bs = toByteString b
    {- FIXME: what do we do when the encoded string is more than 2^32 bytes
    long? -}
    byteSize = fromInt32be . fromIntegral . B.length

encodeString :: String -> Builder
encodeString = prefixLength . fromString

data ClapiValue = CNil | CBool Bool | CTimeTag Int32 Int32 |
    CInt32 Int32 | CInt64 Int64 | CFloat Float | CDouble Double |
    CString String | CList [ClapiValue] deriving (Eq, Show)

encode :: ClapiValue -> (Char, Builder)
encode CNil = ('N', mempty)
encode (CBool True) = ('T', mempty)
encode (CBool False) = ('F', mempty)
encode (CTimeTag x y) = ('t', fromInt32be x <> fromInt32be y)
encode (CInt32 x) = ('i', fromInt32be x)
encode (CInt64 x) = ('h', fromInt64be x)
encode (CFloat x) = ('f', floatBE x)
encode (CDouble x) = ('d', doubleBE x )
encode (CString x) = ('s', encodeString x)
encode (CList vs) = ('l', encodeList vs)

type ArgList = (Builder, Builder)

encode' :: ClapiValue -> ArgList
encode' x = builderify . encode $ x where
    builderify (c, y) = (fromChar c, y)

addArg :: ArgList -> ClapiValue -> ArgList
addArg al cv = al <> encode' cv

encodeList :: [ClapiValue] -> Builder
encodeList vs = prefixLength (fst argList) <> snd argList where
    argList = foldl addArg mempty vs
