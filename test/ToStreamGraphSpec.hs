{- HLINT ignore "Redundant do" -}

module ToStreamGraphSpec where

import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Striot.CompileIoT
import           Striot.StreamGraph
import           Data.Maybe
import           Algebra.Graph

import           ToStreamGraph
import           NRNode


spec :: Spec
spec = do
  describe "toStreamGraph" $ do
    describe "with a single node" $ do
      let nodes =
            [ NRNode "780d3bdd.622694"
                     (Just 1)
                     "filter"
                     Nothing
                     Nothing
                     (Just "filter :: Int -> Bool\nfilter x = x > 5")
                     Nothing
                     Nothing
                     Nothing
                     (Just [[]])
            ]
      let graph = toStreamGraph nodes

      it "does not add the node since it has no connections" $ do
        vertexCount graph `shouldBe` 0
        edgeCount graph `shouldBe` 0

    describe "with two nodes" $ do
      -- each node is connected back to the other
      let nodes =
            [ NRNode "node1"
                     (Just 1)
                     "filter"
                     Nothing
                     Nothing
                     (Just "filter :: Int -> Bool\nfilter x = x > 5")
                     Nothing
                     Nothing
                     Nothing
                     (Just [[2]])
            , NRNode "node2"
                     (Just 2)
                     "filter"
                     Nothing
                     Nothing
                     (Just "filter :: Int -> Bool\nfilter x = x < 3")
                     Nothing
                     Nothing
                     Nothing
                     (Just [[1]])
            ]
      let graph    = toStreamGraph nodes
      let [v1, v2] = vertexList graph

      it "adds the nodes and their connections to the graph" $ do
        -- check the counts are as expected
        vertexCount graph `shouldBe` 2
        edgeCount graph `shouldBe` 2

        -- check the nodes are correctly connected
        let expectedAdjacency = [(v1, [v2]), (v2, [v1])]
        adjacencyList graph `shouldBe` expectedAdjacency


      it "adds the correct type of StreamOperator based on the node type" $ do
        operator v1 `shouldBe` Filter
        operator v2 `shouldBe` Filter

    describe "with multiple nodes" $ do
      it "adds the nodes and their connections to the graph" $ do
        nodes <- nodesFromJSON "test/files/multiple-nodes.json"
        let graph                = toStreamGraph nodes
        let [v1, v2, v3, v4, v5] = vertexList graph

        -- check the counts are as expected
        vertexCount graph `shouldBe` 5
        edgeCount graph `shouldBe` 3

        -- check the nodes are correctly connected
        let expectedAdjacency =
              [(v1, [v3]), (v2, [v5]), (v3, [v4]), (v4, []), (v5, [])]
        adjacencyList graph `shouldBe` expectedAdjacency
