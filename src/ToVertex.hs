{-# LANGUAGE OverloadedStrings #-}

module ToVertex
  ( toVertex
  , nodesFromJSON
  , test2
  , NRNode(..)
  )
where

import           Striot.CompileIoT
import           Striot.StreamGraph
import           Data.Aeson
import           Data.Maybe
import           GHC.Generics
import qualified Data.ByteString.Lazy          as B

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


-- instance FromJSON NRNode

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


-- nodeFromJSON :: [Value] -> (Maybe NRNode)
-- nodeFromJSON x = decode x :: (Maybe NRNode)

nodesFromJSON :: FilePath -> IO ([NRNode])
nodesFromJSON x = fromJust . decode <$> B.readFile x :: IO ([NRNode])

test2 :: Maybe [String]
test2 = decode "[\"1\",\"1\",\"1\"]" :: Maybe [String]

toVertex :: Int -> String -> StreamVertex
toVertex = filterVertex

filterVertex :: Int -> String -> StreamVertex
filterVertex i x = StreamVertex i Filter [x, "s"] "String" "String"
