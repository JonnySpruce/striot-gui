{-# LANGUAGE OverloadedStrings #-}

module ToStreamGraph
  ( toStreamGraph
  , partition
  )
where

import           Algebra.Graph
import           Data.Maybe
import           Striot.CompileIoT
import           Striot.StreamGraph

import           NRNode

toStreamGraph :: [NRNode] -> StreamGraph
toStreamGraph = fromEdgeList . createEdgeList

createEdgeList :: [NRNode] -> [(StreamVertex, StreamVertex)]
createEdgeList [] = []
createEdgeList xs = concatMap
  (\x -> map (\w -> (toVertex x, toVertex (findNode w))) . concat . strWires $ x
  )
  xs
  where findNode = getNodeByStrId xs

fromEdgeList :: [(StreamVertex, StreamVertex)] -> StreamGraph
fromEdgeList = foldr (overlay . edge) empty
  where edge (x, y) = vertex x `connect` vertex y

-- | Converts the node to the correct type of Vertex depending on the node type
toVertex :: NRNode -> StreamVertex
toVertex n = case nodeType n of
  "filter"        -> toVertex' n Filter [func n, "s"]
  "generic-input" -> toVertex' n Source [func n]
  "sink"          -> toVertex' n Sink [func n]
  "map"           -> toVertex' n Map [func n, "s"]

-- | converts a node to a specific StreamVertex to be used by StrIoT
toVertex' :: NRNode -> StreamOperator -> [String] -> StreamVertex
toVertex' n o xs = StreamVertex (strId n)
                                o
                                xs
                                (fromMaybe "String" (input n))
                                (fromMaybe "String" (output n))

partition :: [NRNode] -> [[Int]]
partition xs = [getIds partL, getIds partR]
 where
  getIds = map strId
  partL  = filter (\x -> nodeType x `notElem` ["sink", "generation-options"]) xs
  partR  = filter (\x -> "sink" == nodeType x) xs
