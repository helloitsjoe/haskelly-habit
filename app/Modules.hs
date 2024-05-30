module Modules where

import Data.List (nub, transpose, partition)

numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub

transposed :: [[Int]]
transposed = transpose [[1,2,3],[4,5,6],[7,8,9]]

partitioned :: ([Int], [Int])
partitioned = partition (>3) [1..7]

multiline :: String
multiline = unlines ["first line", "second line", "third line"]

main :: IO ()
main = print multiline
