module Recursion where

fib :: (Num a, Eq a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

max' :: (Ord a) => [a] -> a
max' [] = error "Emtpy list cannot have a maximum"
max' [x] = x
-- max' (x:xs) = if x > max' xs then x else max' xs
max' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = max' xs

replicate' :: (Ord a, Num a) => a -> a -> [a]
-- replicate' 0 _ = []
-- replicate' 1 num = [num]
-- replicate' times num = [num] ++ replicate' (times - 1) num
replicate' times num
  | times <= 0 = []
  | otherwise = num:replicate' (times - 1) num

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' 1 (x:_) = [x]
take' n (x:xs) = x:take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [a] -> [(a, a)]
zip' [] [] = []
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) 
  | x == y = True
  | otherwise = elem' x ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      largerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ largerSorted
  -- if x < y then [x] ++ quicksort (y:xs) else [y] ++ quicksort (x:xs)

main :: IO ()
main = print (quicksort [5, 2, 3, 4, 1])

