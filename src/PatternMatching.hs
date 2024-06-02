module PatternMatching where

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number 7!"
lucky _ = "Sorry!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty!"
tell [x] = "The list has one element: " ++ show x
tell [x,y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long! The first two elements are: " ++ show x ++ " and " ++ show y

-- Pattern matching + recursion
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = length' xs + 1

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

capital :: String -> String
capital "" = "Empty string!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is: " ++ [x]

main :: IO ()
main = print (capital "hello")
