-- node2
import System.Random


sink1 :: Show a => Stream a -> IO ()
sink1 = putStrLn . ("receiving "++) . show . value

streamGraphFn :: Stream String -> Stream String
streamGraphFn n1 = let
    in n1


main :: IO ()
main = nodeSink streamGraphFn sink1 "9001"