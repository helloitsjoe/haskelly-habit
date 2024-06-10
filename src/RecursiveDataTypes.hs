module RecursiveDataTypes where

data List a = Empty | Cons a (List a) deriving (Show, Eq, Read, Ord)

main :: IO ()
main = print "Hello"
