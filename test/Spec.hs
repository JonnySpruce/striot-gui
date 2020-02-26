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
    it "returns a Vertex which can be used to filter" $ do
      n <- nodesFromJSON "test/files/single-node.json"
      ToVertex.id (n !! 0) `shouldBe` "780d3bdd.622694"
      nodeType (n !! 0) `shouldBe` "filter"
      fromJust (z (n !! 0)) `shouldBe` "f1bacb78.71c938"
      fromJust (name (n !! 0)) `shouldBe` ""
      fromJust (func (n !! 0))
        `shouldBe` "filter :: Int -> Bool\nfilter x = x > 5"
      fromJust (x (n !! 0)) `shouldBe` 360
      fromJust (y (n !! 0)) `shouldBe` 180
      fromJust (wires (n !! 0)) `shouldBe` [["45a02407.d5b4fc"]]
