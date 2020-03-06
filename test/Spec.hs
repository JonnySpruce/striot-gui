{- HLINT ignore "Redundant do" -}

import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           Striot.CompileIoT
import           Striot.StreamGraph
import           Data.Maybe

import           ToVertex


main :: IO ()
main = hspec $ do
  describe "ToVertex.nodesFromJSON" $ do
    describe "on a single node" $ do
      describe "(filter node)" $ do
        it "correctly reads the data about the node" $ do
          let expectedNode = NRNode
                "780d3bdd.622694"
                (Just 1)
                "filter"
                (Just "f1bacb78.71c938")
                (Just "")
                (Just "filter :: Int -> Bool\nfilter x = x > 5")
                (Just 360)
                (Just 180)
                (Just [["45a02407.d5b4fc"]])
                Nothing

          nodesFromJSON "test/files/single-node.json"
            `shouldReturn` [expectedNode]

    describe "on multiple nodes" $ do
      it "correctly reads data about each node" $ do
        -- only check two nodes - a non-StrIoT node and a filterNode
        let expectedNodes =
              [ NRNode "d8d488a.0d86178"
                       (Just 1)
                       "tab"
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
              , NRNode "60ac5717.97d4b8"
                       (Just 2)
                       "filter"
                       (Just "d8d488a.0d86178")
                       (Just "")
                       (Just "-- node 1\nfilter :: Int -> Bool\n")
                       (Just 390)
                       (Just 200)
                       (Just [["a06f1981.fb6c78"]])
                       Nothing
              ]

        n <- nodesFromJSON "test/files/multiple-nodes.json"
        let actualNodes = take 2 n

        actualNodes `shouldBe` expectedNodes
        -- check that all nodes have been added
        length actualNodes `shouldBe` length expectedNodes
