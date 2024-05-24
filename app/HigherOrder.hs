zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

concatChars :: Char -> Char -> String
concatChars x y = x : [y]

main :: IO ()
main = print (zipWith' concatChars ['a','b'] ['c','d'])
