module Tests where

import Test.HUnit ((@=?))

import Lib (display, parse, BasePath (..))


testDisplayBasePath =
  "/hello/world/" @=? display (BasePath ["hello", "world"])

testParseBasePath =
  Right (BasePath ["hello", "world"]) @=? parse "/hello/world/"
