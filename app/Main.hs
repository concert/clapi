module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import System.Posix.Signals

import Server


main :: IO ()
main =
  mask $ \restore -> do
    a <- async $ mask_ (
      (threadDelay 10000000 >> putStrLn "done")
        `onException` (
            (allowInterrupt >> putStrLn "poke" >> threadDelay 2000000 >> putStrLn "done poking")
                `onException` putStrLn "poke2")
        )
    installHandler keyboardSignal (Catch $ cancel a) Nothing
    restore $ wait a
