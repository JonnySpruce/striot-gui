import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           Striot.CompileIoT
import           Striot.StreamGraph
import           Data.Maybe

import           ToVertex


main :: IO ()
main =
  hspec
    $ describe "ToVertex.nodesFromJSON"
    $ describe "on a single node"
    $ it "correctly reads the data about the node"
    $ do
        n <- nodesFromJSON "test/files/single-node.json"
        let h = head n
        ToVertex.id h `shouldBe` "780d3bdd.622694"
        nodeType h `shouldBe` "filter"
        fromJust (z h) `shouldBe` "f1bacb78.71c938"
        fromJust (name h) `shouldBe` ""
        fromJust (func h) `shouldBe` "filter :: Int -> Bool\nfilter x = x > 5"
        fromJust (x h) `shouldBe` 360
        fromJust (y h) `shouldBe` 180
        fromJust (wires h) `shouldBe` [["45a02407.d5b4fc"]]

    -- describe "multiple nodes" $ do
    --   it "correctly reads the data about the nodes" $ do
    --     n <- nodesFromJSON "test/files/multiple-nodes.json"
    --     let firstNode = head n
    --     ToVertex.id



-- basicNodeCheck :: String -> String -> String -> IO ()
-- basicNodeCheck = do

