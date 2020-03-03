{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module ToVertex
  ( toVertex
  , nodesFromJSON
  , filterStriotNodes
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
    } deriving (Generic, Show)


-- Defines how to read the JSON object and convert into the NRNode
instance FromJSON NRNode where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "nodeType" then "type" else x
    }


-- | Reads the specified file and converts into an array of NRNodes
nodesFromJSON :: FilePath -> IO [NRNode]
nodesFromJSON x = fromJust . decode <$> B.readFile x :: IO [NRNode]

filterStriotNodes :: [NRNode] -> [NRNode]
filterStriotNodes = filter (isJust . z)

-- currently not used
toVertex :: Int -> NRNode -> StreamVertex
toVertex i n | nodeType n == "filter" = filterVertex i n


-- currently not used
filterVertex :: Int -> NRNode -> StreamVertex
filterVertex i x =
  StreamVertex i Filter [fromJust (func x), "s"] "String" "String"
