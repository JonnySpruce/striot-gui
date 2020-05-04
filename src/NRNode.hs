{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}

module NRNode
  ( NRNode(..)
  , nodesFromJSON
  , getNodeByStrId
  , getStrIoTGenerateOpts
  )
where

import           Data.Aeson
import           Control.Applicative
import           Data.Maybe
import           Data.Either
import           GHC.Generics
import           Data.List.Split
import qualified Striot.CompileIoT             as C
import qualified Data.ByteString.Lazy          as B

-- | NRNode - the Haskell data representation of Node-RED nodes
data NRNode =
    NRNode { nrId :: String -- Node-RED ID - as it comes from Node-RED
         , strId :: Int -- StrIoT ID - unique ID that can then be used for creating Vertices for StrIoT
         , nodeType :: String
         , func :: String
         , input :: Maybe String
         , output :: Maybe String
         , wires :: [[String]]
         , strWires :: [[Int]]
         , imports :: String
         , packages :: String
         , optimise :: Bool
    } deriving (Generic, Show, Eq)

-- Defines how to read the JSON object and convert into the NRNode
-- The optimise line is different because Node-RED outputs true (bool) for True, and '' (string) for False. 
--    Aeson cannot easily handle both Bool and String for the same field, so using <|> (which acts as a catch in 
--    a conversion failure) then having a field which is guaranteed to not be found is the best workaround for this
instance FromJSON NRNode where
  parseJSON = withObject "nrnode" $ \o -> do
    nrId     <- o .: "id"
    strId    <- o .:? "strId" .!= (-1)
    nodeType <- o .: "type"
    func     <- o .:? "func" .!= ""
    input    <- o .:? "input" .!= Nothing
    output   <- o .:? "output" .!= Nothing
    wires    <- o .:? "wires" .!= []
    strWires <- o .:? "strWires" .!= []
    imports  <- o .:? "imports" .!= ""
    packages <- o .:? "packages" .!= ""
    optimise <- o .:? "optimise" .!= False <|> o .:? "" .!= False
    return NRNode { .. }


-- | Reads the specified file and converts into an array of NRNodes
nodesFromJSON :: FilePath -> IO [NRNode]
nodesFromJSON x =
  addInputType
  .   addStrIds
  .   filterActualNodes
  .   fromMaybe (error "Unable to convert JSON file.")
  .   decode
  <$> B.readFile x :: IO [NRNode]

-- | Removes all nodes from the array which represent tabs etc.
filterActualNodes :: [NRNode] -> [NRNode]
filterActualNodes = filter (\n -> nodeType n `elem` striotTypes)

-- | Creates StrIoT compatible nodes for each ID, and also creates wires in the same format
addStrIds :: [NRNode] -> [NRNode]
addStrIds = updateStrWires . zipWith (\id x -> x { strId = id }) [1 ..]

addInputType :: [NRNode] -> [NRNode]
addInputType xs = map (\x -> x { input = getInputType xs x }) xs

-- | Creates/updates the StrIoT wires based on the Node-RED ID and places the equivalent StrIoT ID into the StrWires field
-- | Also removes any references which are not found in the list of IDs (0 or less)
updateStrWires :: [NRNode] -> [NRNode]
updateStrWires xs = map
  (\x -> x { strWires = map (filter (> 0) . map getId) (wires x) })
  xs
  where getId = getStrId xs

-- | Tries to find the StrId for a Node given its NrID
getStrId :: [NRNode] -> String -> Int
getStrId xs id | null results = -1
               | otherwise    = strId . head $ results
  where results = filter (\x -> id == nrId x) xs

-- | Finds a specific node in an array of NRNodes which has the specified strID
-- TODO: add error handling for if node not found 
getNodeByStrId :: [NRNode] -> Int -> NRNode
getNodeByStrId xs i = case results of
  []      -> error ("No node found with ID " ++ show i)
  (x : _) -> x
  where results = filter ((i ==) . strId) xs

-- | Finds the input type for a specified node by finding the input node and returning the type
getInputType :: [NRNode] -> NRNode -> Maybe String
getInputType xs n = case inputs of
  [] -> Nothing
  _  -> output . head $ inputs
  where inputs = filter (\x -> strId n `elem` (concat . strWires $ x)) xs

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
    (map (unwords . words) (endBy "," $ imports x))
    (map (unwords . words) (endBy "," $ packages x))
    ps
    (optimise x)
  | otherwise = error "Node must be of type 'generation-options'"
  where ps = if func x == "" then Nothing else Just (func x)


striotTypes :: [String]
striotTypes = ["filter", "generic-input", "sink", "generation-options", "map"]
