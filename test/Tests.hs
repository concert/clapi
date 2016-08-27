module Tests where

import Test.HUnit ((@=?))

import Lib (display, BasePath (BasePath))


testSerialiseBasePath =
  display (BasePath ["hello", "world"]) @=? "/hello/world/"
