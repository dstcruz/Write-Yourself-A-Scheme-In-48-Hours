module Main where
import System.Environment (getArgs)

main :: IO ()
main = do args <- fmap (fmap read) getArgs
          (putStrLn . show) ((args !! 0) + (args !! 1))