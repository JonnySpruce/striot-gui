{- HLINT ignore "Redundant do" -}

module NRNodeSpec where

import           Test.Hspec
import           Control.Exception              ( evaluate )
import qualified Striot.CompileIoT             as C
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
                Nothing
                Nothing
                Nothing

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
              Nothing
              Nothing
              Nothing

          nodesFromJSON "test/files/single-node/generic-input.json"
            `shouldReturn` [expectedNode]

      describe "(sink node)" $ do
        it "correctly reads the data about the node" $ do
          let expectedNode = NRNode
                "6be7f1eb.edafc"
                (Just 1)
                "sink"
                (Just "mapM_ $ putStrLn . (\"receiving \"++) . show . value")
                Nothing
                (Just "IO ()")
                (Just [])
                (Just [])
                Nothing
                Nothing
                Nothing

          nodesFromJSON "test/files/single-node/sink.json"
            `shouldReturn` [expectedNode]

      describe "(generation options node)" $ do
        let
          expectedNode = NRNode
            "4b18bbc2.dcad64"
            (Just 1)
            "generation-options"
            (Just "-- code goes here")
            Nothing
            Nothing
            (Just [])
            (Just [])
            (Just
              "Striot.FunctionalIoTtypes, Striot.FunctionalProcessing, Striot.Nodes"
            )
            (Just "random, example")
            (Just True)

        it "correctly reads the data about the node" $ do

          nodesFromJSON "test/files/single-node/generation-options.json"
            `shouldReturn` [expectedNode]

    describe "on multiple nodes" $ do
      it
          "correctly reads data about each node and discards the unnecessary nodes"
        $ do
        -- only check two nodes
            let
              expectedNodes =
                [ NRNode
                  "4b18bbc2.dcad64"
                  (Just 1)
                  "generation-options"
                  (Just "-- code goes here")
                  Nothing
                  Nothing
                  (Just [])
                  (Just [])
                  (Just
                    "Striot.FunctionalIoTtypes, Striot.FunctionalProcessing, Striot.Nodes"
                  )
                  (Just "random, example")
                  (Just True)
                , NRNode
                  "5f773d75.f7a804"
                  (Just 2) -- while there is another node first, it is a tab node so not relevant
                  "generic-input"
                  (Just
                    "do\n    i <- getStdRandom (randomR (1,10)) :: IO Int\n    let s = show i in do\n        threadDelay 1000000\n        putStrLn $ \"client sending \" ++ s\n        return s"
                  )
                  Nothing
                  (Just "String")
                  (Just [["f196f84d.7140e8"]])
                  (Just [[3]]) -- The ID above is for the second StrIoT node in the file
                  Nothing
                  Nothing
                  Nothing
                , NRNode "f196f84d.7140e8"
                         (Just 3)
                         "filter"
                         (Just "(\\i -> (read i :: Int) > 5)")
                         (Just "String")
                         (Just "String")
                         (Just [["6be7f1eb.edafc"]])
                         (Just [[4]])
                         Nothing
                         Nothing
                         Nothing
                , NRNode
                  "6be7f1eb.edafc"
                  (Just 4)
                  "sink"
                  (Just "mapM_ $ putStrLn . (\"receiving \"++) . show . value")
                  (Just "String")
                  (Just "IO ()")
                  (Just [])
                  (Just [])
                  Nothing
                  Nothing
                  Nothing
                ]

            actualNodes <- nodesFromJSON "test/files/multiple-node-types.json"
            -- check that all nodes have been added
            length actualNodes `shouldBe` 4

            actualNodes `shouldBe` expectedNodes

      it
          "correctly determines the input types based on the previous node's output type"
        $ do
            nodes <- nodesFromJSON "test/files/complex-connections.json"
            let (n1 : n2 : _) = nodes

            input n1 `shouldBe` Nothing
            output n1 `shouldBe` Just "Int"

            input n2 `shouldBe` Just "Int"
            output n2 `shouldBe` Just "String"

  describe "getStrIoTGenerateOpts" $ do
    let n = nodesFromJSON "test/files/multiple-node-types.json"

    it "converts the node to the correct GenerateOps type" $ do
      let expectedOpts = C.GenerateOpts
            [ "Striot.FunctionalIoTtypes"
            , "Striot.FunctionalProcessing"
            , "Striot.Nodes"
            ]
            ["random", "example"]
            (Just "-- code goes here")
            True

      nodes <- n

      let opts = getStrIoTGenerateOpts nodes

      -- since GenerateOpts doesn't derive from Eq, can't compare directly
      compareOpts opts expectedOpts

    it "correctly handles an empty string for the preSource" $ do
      let expectedOpts = C.GenerateOpts
            [ "Striot.FunctionalIoTtypes"
            , "Striot.FunctionalProcessing"
            , "Striot.Nodes"
            ]
            ["random", "example"]
            Nothing
            True

      nodes <- n

      -- remove the func value for nodes to produce empty preSource for generate-opts node
      let updatedNodes = map (\x -> x { func = Just "" }) nodes
      let opts         = getStrIoTGenerateOpts updatedNodes

      compareOpts opts expectedOpts

    it "correctly handles no imports" $ do
      let expectedOpts = C.GenerateOpts []
                                        ["random", "example"]
                                        (Just "-- code goes here")
                                        True

      nodes <- n

      -- remove the func value for nodes to produce empty preSource for generate-opts node
      let updatedNodes = map (\x -> x { imports = Just "" }) nodes
      let opts         = getStrIoTGenerateOpts updatedNodes

      compareOpts opts expectedOpts

compareOpts :: C.GenerateOpts -> C.GenerateOpts -> IO ()
compareOpts a b = do
  C.imports a `shouldBe` C.imports b
  C.packages a `shouldBe` C.packages b
  C.preSource a `shouldBe` C.preSource b
  C.rewrite a `shouldBe` C.rewrite b
