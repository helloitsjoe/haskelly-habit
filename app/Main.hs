module Main where

-- Basic functions

doubleMe :: Int -> Int
doubleMe x = x + x

-- doubleUs :: Num a => a -> a -> a
doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: Int -> Int
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

firstFiveInfinite :: [Int]
firstFiveInfinite = take 5 [1..]

firstFiveCycle :: String
firstFiveCycle = take 5 (cycle "ABC")

firstFiveRepeat :: [Int]
-- firstFiveRepeat = take 5 (repeat 3)
firstFiveRepeat = replicate 5 3

-- List comprehensions

firstTenDoubled :: [Int]
firstTenDoubled = [x*2 | x <- [1..10]]

fiftyToHundredModSevenThree :: [Int]
fiftyToHundredModSevenThree = [ x | x <- [50..100], x `mod` 7 == 3]

bangBoom :: (Ord xs, Integral xs, Num xs) => [xs] -> [String]
bangBoom xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

multiplePredicates :: [Int]
multiplePredicates = [ x | x <- [1..20], x /= 5, x /= 12, x /= 18]

multipleInputLists :: [Int]
multipleInputLists = [x*y | x <- [2, 4, 5], y <- [3, 6, 9]]

myLength :: [Int] -> Int
myLength xs = sum [ 1 | _ <- xs ]

nested :: [[Int]]
nested = [[1..10], [11..20], [21..30]]
nestedEven :: [[Int]] -> [[Int]]
nestedEven xxs = [[ x | x <- xs, even x] | xs <- xxs]

main :: IO ()
main = print (nestedEven nested)
