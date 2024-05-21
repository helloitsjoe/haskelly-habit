coffeeToWaterRatio :: (Fractional a, Ord a) => a -> a-> String
coffeeToWaterRatio coffee water
  | ratio >= weak = "Too weak!"
  | ratio >= almost = "Getting there..."
  | ratio >= right = "That's about right."
  | otherwise = "Too strong!"
  -- names in `where` have to be aligned
  where ratio = water / coffee
        weak = 20
        almost = 17
        right = 14
        -- alternatively, using pattern matching:
        -- (weak, almost, right) = (20, 17, 14)

max' :: (Ord a) => a -> a -> a
max' a b
  | a < b = b
  | otherwise = a

-- Can also define infix functions with backticks
compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
  | a < b = LT
  | a > b = GT
  | otherwise = EQ

getInitials :: String -> String -> String
getInitials first last = [f] ++ " " ++ [l]
  where (f:_) = first
        (l:_) = last

calcCoffeeRatio :: (Fractional a, Ord a) => [(a, a)] -> [a]
calcCoffeeRatio xs = [getRatio c w | (c, w) <- xs]
  where getRatio c w = w / c

-- `let`/`in` bindings
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let topArea = pi * r ^ 2
      sideArea = 2 * pi * r * h
  in topArea * 2 + sideArea

sumTriple :: (Num a) => (a, a, a) -> a
sumTriple tup = let (a, b, c) = tup in (a + b + c)

-- define scoped functions inline with let
squareTriple :: (Num a) => (a, a, a) -> [(a, a, a)]
squareTriple (a, b, c) = [let square x = x * x in (square a, square b, square c)]

-- Same coffee / water ratio calculation but with a list comprehension
calcCoffeeRatioComprehension :: (RealFloat a) => [(a, a)] -> [a]
calcCoffeeRatioComprehension xs = [calc | (c, w) <- xs, let calc = w / c]

-- case expressions
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a single element long."
                                               xs -> "longer than a single element."

-- alternative using where
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where what [] = "empty."
        what [x] = "a single element long."
        what xs = "longer than a single element."

main :: IO ()
main = print (squareTriple (1, 2, 3))
