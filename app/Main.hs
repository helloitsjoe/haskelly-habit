module Main where

doubleMe :: Num a => a -> a
doubleMe x = x + x

main :: IO ()
main = print (doubleMe 4 :: Int)
