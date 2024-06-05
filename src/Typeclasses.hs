module Typeclasses (Shape (..), Point (..), surface, nudge, main) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) nudgeX nudgeY = Circle (Point (x + nudgeX) (y + nudgeY)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) nudgeX nudgeY = Rectangle (Point (x1 + nudgeX) (y1 + nudgeY)) (Point (x2 + nudgeX) (y2 + nudgeY))

baseCircle = Circle (Point 0 0)
baseRect width height = Rectangle (Point 0 0) (Point width height)

rect = Rectangle (Point 1 2) (Point 3 4)

-- ew
-- data Person = Person String String Int Float String String deriving (Show)

-- better! Functions are automatically created for each property, e.g. firstName guy
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , favoriteFlavor :: String
                     } deriving (Show)

-- guy = Person "George" "Gefferson" 42 3.5 "1-234-567-8910" "Blue Razzleberry"
guy = Person { firstName="George", lastName="Gefferson", age=42, height=3.5, phoneNumber="1-234-567-8910", favoriteFlavor="Blue Razzleberry"}

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vmult :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vmult` (Vector l m n) = Vector (i * l) (j * m) (k * n)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

vecs = Vector 1 2 3 `scalarMult` Vector 4 5 6

main :: IO ()
main = print vecs
