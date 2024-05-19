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

main :: IO ()
main = print (calcCoffeeRatio [(50, 800)])
