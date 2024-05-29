import Data.List (nub, transpose, partition)

numUnique :: (Eq a) => [a] -> Int
numUnique = length . (nub)

transposed = transpose [[1,2,3],[4,5,6],[7,8,9]]

partitioned = partition (>3) [1..7]

main :: IO ()
main = print (partitioned)
