{- HLINT ignore "Redundant do" -}

module NRNodeSpec where

import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Striot.CompileIoT
import           Striot.StreamGraph
import           Data.Maybe

import           NRNode


spec :: Spec
spec = do
  describe "nodesFromJSON" $ do
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
                (Just [[]]) -- since it refers to another node outside the document it should be removed

          nodesFromJSON "test/files/single-node.json"
            `shouldReturn` [expectedNode]

    describe "on multiple nodes" $ do
      it
          "correctly reads data about each node and discards the unnecessary nodes"
        $ do
        -- only check two nodes - a non-StrIoT node and a filterNode
            let
              expectedNodes =
                [ NRNode "60ac5717.97d4b8"
                         (Just 1) -- while there is another node first, it is a tab node so not relevant
                         "filter"
                         (Just "d8d488a.0d86178")
                         (Just "")
                         (Just "-- node 1\nfilter :: Int -> Bool\n")
                         (Just 390)
                         (Just 200)
                         (Just [["a06f1981.fb6c78"]])
                         (Just [[3]]) -- The ID above is for the third StrIoT node in the file
                , NRNode
                  "e930e.ff4cdcf2"
                  (Just 2)
                  "filter"
                  (Just "d8d488a.0d86178")
                  (Just "")
                  (Just
                    "-- node 4\nfilter :: Int -> Bool\n-- complete your definition here"
                  )
                  (Just 480)
                  (Just 380)
                  (Just [["8d98e1ee.971af"]])
                  (Just [[5]])
                ]

            n <- nodesFromJSON "test/files/multiple-nodes.json"
            -- check that all nodes have been added
            length n `shouldBe` 5

            let actualNodes = take 2 n
            actualNodes `shouldBe` expectedNodes
