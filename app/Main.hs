module Main where

import           System.Environment

import           NRNode
import           ToStreamGraph

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      x <- nodesFromJSON fileName
      let striotNodes = toStreamGraph x
      putStr (show striotNodes)
    _ -> helpText

helpText :: IO ()
helpText = putStrLn "Usage: haskell-gui /path/to/node-red/export"
