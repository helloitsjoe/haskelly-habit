module Typeclasses102 where

-- Redefine Eq with === and /==
class Eq' a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool
  x === y = not (x /== y)
  x /== y = not (x === y)

data TrafficLight = Red | Green | Yellow

-- class is for defining new Typeclasses
-- instance is for making our types instances of Typeclasses
instance Eq' TrafficLight where
  Red === Red = True
  Yellow === Yellow = True
  Green === Green = True
  _ === _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Green = "Green light"
  show Yellow = "Yellow light"

main :: IO ()
main = print [Red, Yellow, Green]
