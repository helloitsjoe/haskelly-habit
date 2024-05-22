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
take' first _
  | first <= 0 = []
take' _ [] = []
take' 1 (x:_) = [x]
take' first (x:xs) = x:take' (first - 1) xs

main :: IO ()
main = print (take' 2 [1, 2, 3])

