{-# LANGUAGE OverloadedStrings #-}

module ToStreamGraph
  ( toStreamGraph
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
  (\x ->
    map (\w -> (toVertex x, toVertex (findNode w)))
      . concat
      . fromJust
      . strWires
      $ x
  )
  xs
  where findNode = getNodeByStrId xs

fromEdgeList :: [(StreamVertex, StreamVertex)] -> StreamGraph
fromEdgeList = foldr (overlay . edge) empty
  where edge (x, y) = vertex x `connect` vertex y

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
