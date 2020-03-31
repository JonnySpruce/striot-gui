module Main where

import           System.Environment
import           Striot.CompileIoT

import           NRNode
import           ToStreamGraph

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      nodes <- nodesFromJSON fileName
      let graph = toStreamGraph nodes
          part  = partition nodes
          opts  = getStrIoTGenerateOpts nodes
      partitionGraph graph part opts

    _ -> helpText

helpText :: IO ()
helpText = putStrLn "Usage: striot-gui-exe /path/to/node-red/export"
