module Typeclasses102 where

-- Redefine Eq with === and /==
class Eq' a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool
  x === y = not (x /== y)
  x /== y = not (x === y)

main :: IO ()
main = print "Hello"
