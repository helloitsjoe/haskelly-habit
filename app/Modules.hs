module Modules where

import Data.List (nub, transpose, partition, intersect)
import Data.Char as C

numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub

transposed :: [[Int]]
transposed = transpose [[1,2,3],[4,5,6],[7,8,9]]

partitioned :: ([Int], [Int])
partitioned = partition (>3) [1..7]

multiline :: String
multiline = unlines ["first line", "second line", "third line"]

wordArray :: [String]
wordArray = words "Turn me into an array"

sentence :: String
sentence = unwords ["Turn", "me", "into", "a", "sentence"]

intersectingInts :: [Int]
intersectingInts = [1..7] `intersect` [5..10]

intersectingWords :: String
-- This does not produce what you would expect!
intersectingWords = "Hey man nice" `intersect` "man nice shot"

ints :: [Int]
ints = map C.digitToInt "1234abc"

digits :: String
digits = map C.intToDigit [1, 2, 3, 15]

cats :: [GeneralCategory]
cats = map C.generalCategory " \t\nAb14*"

-- Similar to charCodeAt in JS
ords :: [Int]
ords = map ord "123azAZ"

-- Similar to fromCharCode in JS
chrs :: String
chrs = map chr [90, 65]

caesarize :: Int -> String -> String
caesarize shift text =
  let chars = map ord text
      shifted = map (+ shift) chars
  in map chr shifted
-- With composition: caesarize shift = map (chr . (+ shift) . ord) 

uncaesarize :: Int -> String -> String
uncaesarize shift = caesarize (negate shift)

main :: IO ()
main = print (uncaesarize 2 (caesarize 2 "hello"))
