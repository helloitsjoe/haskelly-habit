quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  -- These lambdas are unnecessary
  let less = quicksort (filter (\y -> y < x) xs)
      more = quicksort (filter (\y -> y >= x) xs)
  in less ++ [x] ++ more

addTuples :: (Num a) => [(a, a)] -> [a]
addTuples xs = map (\(a, b) -> a + b) xs

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-- Alternatively:
-- sum' = foldl (+) 0

main :: IO ()
main = print (sum' [1, 2, 3])
