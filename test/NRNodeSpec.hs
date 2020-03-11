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
                (Just "filter :: Int -> Bool\nfilter x = x > 5")
                Nothing
                (Just "String")
                (Just [["45a02407.d5b4fc"]])
                (Just [[]]) -- since it refers to another node outside the document it should be removed

          nodesFromJSON "test/files/single-node.json"
            `shouldReturn` [expectedNode]

    describe "on multiple nodes" $ do
      it
          "correctly reads data about each node and discards the unnecessary nodes"
        $ do
        -- only check two nodes
            let
              expectedNodes =
                [ NRNode
                  "e0124a9b.057a88"
                  (Just 1) -- while there is another node first, it is a tab node so not relevant
                  "filter"
                  (Just
                    "filter :: Int -> Bool\n-- complete your definition here"
                  )
                  Nothing
                  (Just "Int")
                  (Just [["a8ad8cf4.7b7a5"]])
                  (Just [[2]]) -- The ID above is for the second StrIoT node in the file
                , NRNode
                  "a8ad8cf4.7b7a5"
                  (Just 2)
                  "filter"
                  (Just
                    "filter :: Int -> Bool\n-- complete your definition here"
                  )
                  (Just "Int")
                  (Just "String")
                  (Just [["265bc0de.a1e9c"]])
                  (Just [[3]])
                ]

            n <- nodesFromJSON "test/files/multiple-nodes.json"
            -- check that all nodes have been added
            length n `shouldBe` 5

            let actualNodes = take 2 n
            actualNodes `shouldBe` expectedNodes
