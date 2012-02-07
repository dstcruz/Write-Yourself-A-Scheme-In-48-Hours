module Main where

main :: IO ()
main = do putStrLn "What be yer name?"
          name <- getLine
          putStrLn ("Well, howdy there " ++ name)