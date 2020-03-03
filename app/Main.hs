module Main where

import           System.Environment


import           ToVertex

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      x <- nodesFromJSON fileName
      let striotNodes = filterStriotNodes x
      putStr (show striotNodes)
    _ -> helpText

helpText :: IO ()
helpText = putStrLn "Usage: haskell-gui /path/to/node-red/export"
