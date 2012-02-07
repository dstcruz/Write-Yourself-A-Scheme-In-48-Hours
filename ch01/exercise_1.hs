module Main where

import System.Environment (getArgs)

main :: IO ()
main = do args <- getArgs
          putStrLn ("Hello: " ++ (args !! 0) ++ ", and " ++ (args !! 1))