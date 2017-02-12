module TestServer where

import Test.HUnit (assertEqual, assertBool)
import Test.Framework.Providers.HUnit (testCase)

import qualified Control.Exception as E
import qualified Network.Socket as NS
import Network.Simple.TCP (HostPreference(HostAny))

import Server (listen')


tests = [
    testCase "zero listen" testListenZeroGivesPort
    ]


testListenZeroGivesPort =
  do
    p <- port
    assertBool "port == 0" $ p /= 0
  where
    getPort (NS.SockAddrInet port _) = port
    getPort (NS.SockAddrInet6 port _ _ _) = port
    port = E.bracket
        (listen' HostAny "0")
        (NS.close . fst)
        (return . getPort . snd)
