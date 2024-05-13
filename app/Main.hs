module Main where

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100
                         then x
                         else x * 2

lostNumbers :: [Int]
lostNumbers = [4, 8, 15, 16, 23, 42]

main :: IO ()
main = print (lostNumbers !! 4)
