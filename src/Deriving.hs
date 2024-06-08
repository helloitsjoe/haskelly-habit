module Deriving where

import Data.List ((\\))

data Person = Person { name :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

guy :: Person
guy = Person {name = "Jim", age = 32}

lady :: Person
lady = Person {name = "Jane", age = 33}

otherGuy :: Person
otherGuy = Person {name = "Jim", age = 32}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Enum, Show, Bounded)

weekdays :: [Day]
weekdays = [Monday .. Friday] :: [Day]

days :: [Day]
days = [minBound .. maxBound] :: [Day]

weekend :: [Day]
weekend = days \\ weekdays -- Difference

-- Type synonyms
type PhoneNumber = String
type Name = String
type PhoneBook = [(PhoneNumber, Name)]

phoneBook :: PhoneBook
phoneBook = [("123-4567", "Jackie")
            , ("234-5678", "James")
            , ("345-6789", "Fred")]

inPhoneBook :: Name -> PhoneBook -> Bool
inPhoneBook n book = elem n $ map snd book

getInPhoneBook :: Name -> PhoneBook -> Maybe PhoneNumber
getInPhoneBook _ [] = Nothing
getInPhoneBook n ((num, maybeName):xs) = if n == maybeName then Just num else getInPhoneBook n xs
  
 
main :: IO ()
main = print (getInPhoneBook "Jackie" phoneBook)
