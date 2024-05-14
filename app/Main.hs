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

fifthLost :: Int
fifthLost = lostNumbers !! 4

lostHead :: Int
lostHead = head lostNumbers

lostTail :: [Int]
lostTail = tail lostNumbers

lostInit :: [Int]
lostInit = init lostNumbers

lostLast :: Int
lostLast = last lostNumbers

lostLen :: Int
lostLen = length lostNumbers

lostFirstThree :: [Int]
lostFirstThree = take 3 lostNumbers

lostLastThree :: [Int]
lostLastThree = drop 3 lostNumbers

lostMin :: Int
lostMin = minimum lostNumbers

lostProduct :: Int
lostProduct = product lostNumbers

lostSixteenExists :: Bool
lostSixteenExists = 16 `elem` lostNumbers

range :: [Int]
range = [1..20]

rangeByThrees :: [Int]
rangeByThrees = [3,6..20]

main :: IO ()
main = print rangeByThrees
