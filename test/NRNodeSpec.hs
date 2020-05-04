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
          let expectedNode = NRNode "780d3bdd.622694"
                                    1
                                    "filter"
                                    "filter :: Int -> Bool\nfilter x = x > 5"
                                    Nothing
                                    (Just "String")
                                    [["45a02407.d5b4fc"]]
                                    [[]] -- since it refers to another node outside the document it should be removed
                                    ""
                                    ""
                                    False

          nodesFromJSON "test/files/single-node/filter.json"
            `shouldReturn` [expectedNode]

      describe "(generic input node)" $ do
        it "correctly reads the data about the node" $ do
          let expectedNode = NRNode
                "657a589a.efb478"
                1
                "generic-input"
                "do\n    threadDelay (1000*1000)\n    return \"Hello World!\""
                Nothing
                (Just "String")
                [[]]
                [[]]
                ""
                ""
                False

          nodesFromJSON "test/files/single-node/generic-input.json"
            `shouldReturn` [expectedNode]

      describe "(map node)" $ do
        it "correctly reads the data about the node" $ do
          let expectedNode = NRNode "7636fdb9.9e7604"
                                    1
                                    "map"
                                    "(\\num -> num*2)"
                                    Nothing
                                    (Just "Int")
                                    [[]]
                                    [[]]
                                    ""
                                    ""
                                    False

          nodesFromJSON "test/files/single-node/map.json"
            `shouldReturn` [expectedNode]

      describe "(sink node)" $ do
        it "correctly reads the data about the node" $ do
          let expectedNode = NRNode
                "6be7f1eb.edafc"
                1
                "sink"
                "mapM_ $ putStrLn . (\"receiving \"++) . show . value"
                Nothing
                (Just "IO ()")
                []
                []
                ""
                ""
                False

          nodesFromJSON "test/files/single-node/sink.json"
            `shouldReturn` [expectedNode]

      describe "(generation options node)" $ do
        let
          expectedNode = NRNode
            "4b18bbc2.dcad64"
            1
            "generation-options"
            "-- code goes here"
            Nothing
            Nothing
            []
            []
            "Striot.FunctionalIoTtypes, Striot.FunctionalProcessing, Striot.Nodes"
            "random, example"
            True

        describe "when optimise is True" $ do
          it "correctly reads the data about the node" $ do
            nodesFromJSON "test/files/single-node/generation-options/true.json"
              `shouldReturn` [expectedNode]


        describe "when optimise is False" $ do
          it "correctly reads the data about the node" $ do
            let updatedNode = expectedNode { optimise = False }
            nodesFromJSON "test/files/single-node/generation-options/false.json"
              `shouldReturn` [updatedNode]

    describe "on multiple nodes" $ do
      it
          "correctly reads data about each node and discards the unnecessary nodes"
        $ do
        -- only check two nodes
            let
              expectedNodes =
                [ NRNode
                  "4b18bbc2.dcad64"
                  1
                  "generation-options"
                  "-- code goes here"
                  Nothing
                  Nothing
                  []
                  []
                  "Striot.FunctionalIoTtypes, Striot.FunctionalProcessing, Striot.Nodes"
                  "random, example"
                  True
                , NRNode
                  "5f773d75.f7a804"
                  2 -- while there is another node first, it is a tab node so not relevant
                  "generic-input"
                  "do\n    i <- getStdRandom (randomR (1,10)) :: IO Int\n    let s = show i in do\n        threadDelay 1000000\n        putStrLn $ \"client sending \" ++ s\n        return s"
                  Nothing
                  (Just "String")
                  [["f196f84d.7140e8"]]
                  [[3]] -- The ID above is for the second StrIoT node in the file
                  ""
                  ""
                  False
                , NRNode "f196f84d.7140e8"
                         3
                         "filter"
                         "(\\i -> (read i :: Int) > 5)"
                         (Just "String")
                         (Just "String")
                         [["6be7f1eb.edafc"]]
                         [[4]]
                         ""
                         ""
                         False
                , NRNode
                  "6be7f1eb.edafc"
                  4
                  "sink"
                  "mapM_ $ putStrLn . (\"receiving \"++) . show . value"
                  (Just "String")
                  (Just "IO ()")
                  []
                  []
                  ""
                  ""
                  False
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
      let updatedNodes = map (\x -> x { func = "" }) nodes
      let opts         = getStrIoTGenerateOpts updatedNodes

      compareOpts opts expectedOpts

    it "correctly handles no imports" $ do
      let expectedOpts = C.GenerateOpts []
                                        ["random", "example"]
                                        (Just "-- code goes here")
                                        True

      nodes <- n

      -- remove the func value for nodes to produce empty preSource for generate-opts node
      let updatedNodes = map (\x -> x { imports = "" }) nodes
      let opts         = getStrIoTGenerateOpts updatedNodes

      compareOpts opts expectedOpts

compareOpts :: C.GenerateOpts -> C.GenerateOpts -> IO ()
compareOpts a b = do
  C.imports a `shouldBe` C.imports b
  C.packages a `shouldBe` C.packages b
  C.preSource a `shouldBe` C.preSource b
  C.rewrite a `shouldBe` C.rewrite b
