module Main where

import Server (runServer)
import System.Environment (getArgs)

main :: IO ()
main = do
  arguments <- getArgs
  let port = case arguments of
        requestedPort : _ -> requestedPort
        [] -> "3000"
  runServer port
