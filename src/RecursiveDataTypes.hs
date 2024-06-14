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

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeInsert :: (Num a, Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node y left right)
  | x < y = Node y (treeInsert x left) right
  | x > y = Node y left (treeInsert x right) 
  | x == y = Node x left right
treeInsert _ (Node {}) = EmptyTree

treeCreate :: (Num a, Ord a) => [a] -> Tree a
-- treeCreate = foldl (\ acc c -> treeInsert c acc) EmptyTree
treeCreate = foldr treeInsert EmptyTree

-- tree = treeInsert 5 (treeInsert 4 (treeInsert 3 EmptyTree))
tree = treeCreate [3, 4, 1]

main :: IO ()
main = print tree
