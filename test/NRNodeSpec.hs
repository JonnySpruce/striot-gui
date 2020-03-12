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

          nodesFromJSON "test/files/single-node/filter.json"
            `shouldReturn` [expectedNode]

      describe "(generic input node)" $ do
        it "correctly reads the data about the node" $ do
          let
            expectedNode = NRNode
              "657a589a.efb478"
              (Just 1)
              "generic-input"
              (Just
                "do\n    threadDelay (1000*1000)\n    return \"Hello World!\""
              )
              Nothing
              (Just "String")
              (Just [[]])
              (Just [[]])

          nodesFromJSON "test/files/single-node/generic-input.json"
            `shouldReturn` [expectedNode]

    describe "on multiple nodes" $ do
      it
          "correctly reads data about each node and discards the unnecessary nodes"
        $ do
        -- only check two nodes
            let
              expectedNodes =
                [ NRNode
                  "53d73312.aa3aec"
                  (Just 1) -- while there is another node first, it is a tab node so not relevant
                  "generic-input"
                  (Just
                    "do\n    threadDelay (1000*1000)\n    return \"Hello World!\""
                  )
                  Nothing
                  (Just "String")
                  (Just [["e2841e3f.0d379"]])
                  (Just [[2]]) -- The ID above is for the second StrIoT node in the file
                , NRNode "e2841e3f.0d379"
                         (Just 2)
                         "filter"
                         (Just "(\\i -> (read i :: Int) > 5)")
                         (Just "String")
                         (Just "String")
                         (Just [["fad96b10.65b4a8"]])
                         (Just [[3]])
                ]

            n <- nodesFromJSON "test/files/multiple-node-types.json"
            -- check that all nodes have been added
            length n `shouldBe` 3

            let actualNodes = take 2 n
            actualNodes `shouldBe` expectedNodes

      it
          "correctly determines the input types based on the previous node's output type"
        $ do
            nodes <- nodesFromJSON "test/files/complex-connections.json"
            print nodes
            let (n1 : n2 : _) = nodes

            input n1 `shouldBe` Nothing
            output n1 `shouldBe` Just "Int"

            input n2 `shouldBe` Just "Int"
            output n2 `shouldBe` Just "String"
