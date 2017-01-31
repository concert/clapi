module Main where

import Server

main :: IO ()
main = serve relayWorker action 1234
