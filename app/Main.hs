module Main where

import           System.Environment


import           ToVertex

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      x <- nodesFromJSON fileName
      putStr (show x)
    _ -> helpText

helpText :: IO ()
helpText = putStrLn "Usage: haskell-gui /path/to/node-red/export"
