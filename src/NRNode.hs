{-# LANGUAGE DeriveGeneric #-}

module NRNode
  ( NRNode(..)
  , nodesFromJSON
  , getNodeByStrId
  , getStrIoTGenerateOpts
  )
where

import           Data.Aeson
import           Data.Maybe
import           GHC.Generics
import           Data.List.Split
import qualified Striot.CompileIoT             as C
import qualified Data.ByteString.Lazy          as B

-- | NRNode - the Haskell data representation of Node-RED nodes
data NRNode =
    NRNode { nrId :: String -- Node-RED ID - as it comes from Node-RED
         , strId :: Maybe Int -- StrIoT ID - unique ID that can then be used for creating Vertices for StrIoT
         , nodeType :: String
         , func :: Maybe String
         , input :: Maybe String
         , output :: Maybe String
         , wires :: Maybe [[String]]
         , strWires :: Maybe [[Int]]
         , imports :: Maybe String
         , packages :: Maybe String
         , optimise :: Maybe Bool
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
nodesFromJSON x =
  addInputType
  .   addStrIds
  .   filterActualNodes
  .   fromJust
  .   decode
  <$> B.readFile x :: IO [NRNode]

-- | Removes all nodes from the array which represent tabs etc.
filterActualNodes :: [NRNode] -> [NRNode]
filterActualNodes = filter (\n -> nodeType n `elem` striotTypes)

-- | Creates StrIoT compatible nodes for each ID, and also creates wires in the same format
addStrIds :: [NRNode] -> [NRNode]
addStrIds = updateStrWires . zipWith (\id x -> x { strId = Just id }) [1 ..]

addInputType :: [NRNode] -> [NRNode]
addInputType xs = map (\x -> x { input = getInputType xs x }) xs

-- | Creates/updates the StrIoT wires based on the Node-RED ID and places the equivalent StrIoT ID into the StrWires field
-- | Also removes any references which are not found in the list of IDs (0 or less)
updateStrWires :: [NRNode] -> [NRNode]
updateStrWires xs = map
  (\x -> x
    { strWires = Just (map (filter (> 0) . map getId) (fromJust (wires x)))
    }
  )
  xs
  where getId = getStrId xs

-- | Tries to find the StrId for a Node given its NrID
getStrId :: [NRNode] -> String -> Int
getStrId xs id | null results = -1
               | otherwise    = fromJust . strId . head $ results
  where results = filter (\x -> id == nrId x) xs

-- | Finds a specific node in an array of NRNodes which has the specified strID
-- TODO: add error handling for if node not found 
getNodeByStrId :: [NRNode] -> Int -> NRNode
getNodeByStrId xs i = head . filter (\x -> fromJust (strId x) == i) $ xs

-- | Finds the input type for a specified node by finding the input node and returning the type
getInputType :: [NRNode] -> NRNode -> Maybe String
getInputType xs n = case inputs of
  [] -> Nothing
  _  -> output . head $ inputs
 where
  inputs = filter
    (\x -> (fromJust . strId $ n) `elem` (concat . fromJust . strWires $ x))
    xs

-- | Creates a GenerateOpts datatype from a list of NRNodes (where one is the 'generation-options node type')
getStrIoTGenerateOpts :: [NRNode] -> C.GenerateOpts
getStrIoTGenerateOpts xs = toStrIoTGenerateOpts optsNode
 where
  optsNode = fromMaybe (error "No generation-options node found")
                       (getGenerationOptsNode xs)

-- | Gets the generation settings node from a list of nodes 
getGenerationOptsNode :: [NRNode] -> Maybe NRNode
getGenerationOptsNode xs = case results of
  [] -> Nothing
  _  -> Just . head $ results
  where results = filter (\x -> "generation-options" == nodeType x) xs

toStrIoTGenerateOpts :: NRNode -> C.GenerateOpts
toStrIoTGenerateOpts x
  | nodeType x == "generation-options" = C.GenerateOpts
    (map (unwords . words) (endBy "," $ fromJust . imports $ x))
    (map (unwords . words) (endBy "," $ fromJust . packages $ x))
    ps
    (fromMaybe True . optimise $ x)
  | otherwise = error "Node must be of type 'generation-options'"
 where
  ps        = if preSource == "" then Nothing else Just preSource
  preSource = fromJust (func x)


striotTypes :: [String]
striotTypes = ["filter", "generic-input", "sink", "generation-options"]
