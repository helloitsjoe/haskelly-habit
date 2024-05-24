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
flip' a = b
  where b x y = a y x

subSecondFromFirst :: (Num a) => a -> a -> a
subSecondFromFirst x y = x - y

subFirstFromSecond :: (Num a) => a -> a -> a
subFirstFromSecond = flip' subSecondFromFirst

main :: IO ()
main = print (subFirstFromSecond 2 1)
