module TestPath where

import Test.HUnit ((@=?), assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe (isNothing)
import Data.Word (Word16)
import qualified Data.Map.Strict as Map

import Helpers (assertFailed)
import Clapi.Path (Path, root, up)
import Path.Parsing (toString, fromString)
import Clapi.Types (
    CanFail, Message(..), Time(..), ClapiValue(..), ClapiMethod(..),
    Interpolation(..), fromClapiValue, toClapiValue)
import Clapi.Serialisation (encode, decode)


tests = [
    testCase "path fromString" testPathFromString,
    testCase "up" testUp
    ]

testPathFromString =
  do
    assertFailed "empty" (fromString "")
    fromString "/" >>= assertEqual "slash" []
    fromString "/foo" >>= assertEqual "single" ["foo"]
    fromString "/foo/bar" >>= assertEqual "multiple" ["foo", "bar"]
    -- FIXME: should the following strictly be an error?
    -- assertFailed "trailing" (fromString "/foo/bar/")
    assertFailed "no leading" (fromString "foo/bar")


testUp =
  do
    assertEqual "root up failed" root $ up root
    assertEqual "normal up failed" ["a"] $ up ["a", "b"]
