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
                     (Just "filter :: Int -> Bool\nfilter x = x > 5")
                     Nothing
                     (Just "String")
                     (Just [[]])
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
                     (Just "filter :: Int -> Bool\nfilter x = x > 5")
                     Nothing
                     (Just "Int")
                     (Just [["node2"]])
                     (Just [[2]])
            , NRNode "node2"
                     (Just 2)
                     "filter"
                     (Just "filter :: Int -> Bool\nfilter x = x < 3")
                     (Just "Int")
                     (Just "String")
                     (Just [["node1"]])
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


      it "created a StreamVertex with the correct parameters" $ do
        operator v1 `shouldBe` Filter
        operator v2 `shouldBe` Filter

        intype v1 `shouldBe` "String"
        intype v2 `shouldBe` "Int"

        outtype v1 `shouldBe` "Int"
        outtype v2 `shouldBe` "String"

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
              [(v1, [v2]), (v2, [v3]), (v3, []), (v4, [v5]), (v5, [])]
        adjacencyList graph `shouldBe` expectedAdjacency
