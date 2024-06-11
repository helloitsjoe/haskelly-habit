module RecursiveDataTypes where

-- data List a = Empty | Cons a (List a) deriving (Show, Eq, Read, Ord)

-- Redefine Cons as :-:, special characters are infix by default.
infixr 5 :-: -- This defines the fixity of :-:, with infixr being right-associative with a fixity of 5
data List a = Empty | a :-: (List a) deriving (Show, Eq, Read, Ord)

-- Reimplement ++
infixr 5 .++.
(.++.) :: List a -> List a -> List a
Empty .++. xs = xs
(z :-: zs) .++. as  = z :-: (zs .++. as)

main :: IO ()
main = print (3 :-: (1 :-: Empty))
