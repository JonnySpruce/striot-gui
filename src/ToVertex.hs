{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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
         , z :: String
         , name :: String
         , func :: String
         , x :: Int
         , y :: Int
         , wires :: [[String]]
    } deriving (Generic, Show)


-- Defines how to read the JSON object and convert into the NRNode
instance FromJSON NRNode where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "nodeType" then "type" else x
    }


-- | Reads the specified file and converts into an array of NRNodes
nodesFromJSON :: FilePath -> IO [NRNode]
nodesFromJSON x = fromJust . decode <$> B.readFile x :: IO [NRNode]

removeNullNodes :: [Maybe NRNode] -> [NRNode]
removeNullNodes (Nothing : xs) = removeNullNodes xs
removeNullNodes (x       : xs) = fromJust x : removeNullNodes xs

-- currently not used
toVertex :: Int -> NRNode -> StreamVertex
toVertex i n | nodeType n == "filter" = filterVertex i n


-- currently not used
filterVertex :: Int -> NRNode -> StreamVertex
filterVertex i x = StreamVertex i Filter [func x, "s"] "String" "String"
