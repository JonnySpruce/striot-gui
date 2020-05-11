-- node1
import System.Random


src1 = do
    i <- getStdRandom (randomR (1,10)) :: IO Int
    let s = i in do
        threadDelay 1000000
        putStrLn $ "number generated: " ++ (show s)
        return s

streamGraphFn :: Stream Int -> Stream String
streamGraphFn n1 = let
    n2 = (\s -> streamFilter (\i -> i > 5 s) n1
    in n2


main :: IO ()
main = do
       
       nodeSource src1 streamGraphFn "node2" "9001"