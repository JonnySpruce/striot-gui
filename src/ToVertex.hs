{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module ToVertex
  ( toVertex
  , nodesFromJSON
  , filterActualNodes
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
    NRNode { nrId :: String
         , strId :: Maybe Int
         , nodeType :: String
         , z :: Maybe String
         , name :: Maybe String
         , func :: Maybe String
         , x :: Maybe Int
         , y :: Maybe Int
         , wires :: Maybe [[String]]
         , strWires :: Maybe [[Int]]
    } deriving (Generic, Show, Eq)

-- Defines how to read the JSON object and convert into the NRNode
instance FromJSON NRNode where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \x -> case x of
                             "nrId"     -> "id"
                             "nodeType" -> "type"
                             _          -> x
    }

-- | Reads the specified file and converts into an array of NRNodes
nodesFromJSON :: FilePath -> IO [NRNode]
nodesFromJSON x = addStrIds . fromJust . decode <$> B.readFile x :: IO [NRNode]

-- | Removes all nodes from the array which represent tabs etc.
filterActualNodes :: [NRNode] -> [NRNode]
filterActualNodes = filter (isJust . z)

-- | Creates StrIoT compatible nodes for each ID, and also creates wires in the same format
addStrIds :: [NRNode] -> [NRNode]
addStrIds = updateStrWires . zipWith (\id x -> x { strId = Just id }) [1 ..]

-- | Creates/updates the StrIoT wires based on the Node-RED ID and places the equivalent StrIoT ID into the StrWires field
updateStrWires :: [NRNode] -> [NRNode]
updateStrWires [] = []
updateStrWires (x : xs) =
  x { strWires = Just (map (map getId) (fromJust (wires x))) }
    : updateStrWires xs
  where getId = getStrId (x : xs)

getStrId :: [NRNode] -> String -> Int
getStrId xs id = fromJust . strId . head . filter (\x -> nrId x == id) $ xs

-- | Converts the node to the correct type of Vertex depending on the node type
toVertex :: NRNode -> StreamVertex
toVertex n | nodeType n == "filter" = filterVertex n

-- | converts a node to a filter vertex to be used by StrIoT
filterVertex :: NRNode -> StreamVertex
filterVertex x = StreamVertex (fromJust . strId $ x)
                              Filter
                              [fromJust (func x), "s"]
                              "String"
                              "String"
