module HigherOrder where

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

concatChars :: Char -> Char -> String
concatChars x y = x : [y]

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

addThree :: (Num a) => a -> a
addThree x = x + 3

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

subSecondFromFirst :: (Num a) => a -> a -> a
subSecondFromFirst x y = x - y

subFirstFromSecond :: (Num a) => a -> a -> a
subFirstFromSecond = flip' subSecondFromFirst

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let less = quicksort (filter (<x) xs)
      more = quicksort (filter (>=x) xs)
  in less ++ [x] ++ more

largestUnder100kDivisible :: Int
largestUnder100kDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

sumOfOddSquaresSmallerThanTenK :: Int
sumOfOddSquaresSmallerThanTenK = sum (takeWhile (<10000) (map g [1,3..]))
  where g x = x ^ 2

collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
  | even n = n:collatzChain (n `div` 2)
  | odd n = n:collatzChain (n * 3 + 1)

chainsGT15 =  length (filter f [1..100])
  where f x = length (collatzChain x) > 15

main :: IO ()
main = print (chainsGT15)
