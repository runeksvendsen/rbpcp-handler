module Main where

import qualified RBPCP.App as App

main :: IO ()
main = App.runTestApp testPort
    where testPort = 8080
