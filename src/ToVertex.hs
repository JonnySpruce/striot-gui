{-# LANGUAGE OverloadedStrings #-}

module ToVertex
  ( toVertex
  , nodesFromJSON
  , NRNode(..)
  )
where

import           Striot.CompileIoT
import           Striot.StreamGraph
import           Data.Aeson
import           Data.Maybe
import           GHC.Generics
import qualified Data.ByteString.Lazy          as B

-- | NRNode - the Haskell data representation of Node-RED nodes
data NRNode =
    NRNode { id :: String
         , nodeType :: String
         , z :: Maybe String
         , name :: Maybe String
         , func :: Maybe String
         , x :: Maybe Int
         , y :: Maybe Int
         , wires :: Maybe [[String]]
    } deriving (Show)


-- Defines how to read the JSON object and convert into the NRNode
instance FromJSON NRNode where
  parseJSON (Object v) =
    NRNode
      <$> v
      .:  "id"
      <*> v
      .:  "type"
      <*> v
      .:? "z"
      <*> v
      .:? "name"
      <*> v
      .:? "func"
      <*> v
      .:? "x"
      <*> v
      .:? "y"
      <*> v
      .:? "wires"

-- | Reads the specified file and converts into an array of NRNodes
nodesFromJSON :: FilePath -> IO [NRNode]
nodesFromJSON x = fromJust . decode <$> B.readFile x :: IO [NRNode]

-- currently not used
toVertex :: Int -> String -> StreamVertex
toVertex = filterVertex

-- currently not used
filterVertex :: Int -> String -> StreamVertex
filterVertex i x = StreamVertex i Filter [x, "s"] "String" "String"
