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

main :: IO ()
main = print (head [1, 2, 3])
