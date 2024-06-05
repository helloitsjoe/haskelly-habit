module Deriving where

data Person = Person { name :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

guy = Person {name = "Jim", age = 32}
lady = Person {name = "Jane", age = 33}
otherGuy = Person {name = "Jim", age = 32}

main :: IO ()
main = print (show (guy == otherGuy))
