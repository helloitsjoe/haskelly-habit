module Main where

main :: IO ()
-- main = putStrLn "Hello, Haskell!"
main = putStrLn ("Even numbers:" ++ show (take 10 (filter even [43..])))
