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

map' :: (a -> b) -> [a] -> [b]
-- foldr is used for map because prepending with : is much less expensive than
-- appending with ++... also note that acc/curr params in foldr are reverse
-- order of foldl (reduce in JS)
map' fn xs = foldr (\curr acc -> fn curr : acc) [] xs

-- How many elements does it take for the sum of the roots of all natural
-- numbers to exceed 1000?
numRoots :: Int
numRoots = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- function application with $
numRootsDollar :: Int
numRootsDollar = length (takeWhile (<1000) $ scanl1 (+) $ map sqrt [1..]) + 1

-- function composition with .
negateMap = map (\x -> negate (abs x))
-- negateMap = map (negate . abs)

numRootsComposed :: Int
numRootsComposed = (length . takeWhile (<1000) . scanl1 (+) . map sqrt $ [1..]) + 1

-- How would we write this in point free style?
-- fn x = ceiling (negate (tan (cos (max 50 x))))
fn = ceiling . negate .tan . cos . max 50

-- sum of all odd squares less than 10000 composed
sumOfOddSquares = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- Maybe more readable with let
sumOfOddSquaresLet :: (Integral a) => a -> a
sumOfOddSquaresLet limit =
  let oddSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (< limit) oddSquares
  in sum belowLimit

main :: IO ()
main = print (sumOfOddSquaresLet 10000)
