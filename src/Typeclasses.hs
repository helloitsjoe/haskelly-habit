module Typeclasses where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) nudgeX nudgeY = Circle (Point (x + nudgeX) (y + nudgeY)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) nudgeX nudgeY = Rectangle (Point (x1 + nudgeX) (y1 + nudgeY)) (Point (x2 + nudgeX) (y2 + nudgeY))

rect = Rectangle (Point 1 2) (Point 3 4)

main :: IO ()
main = print (nudge rect 1 2)
