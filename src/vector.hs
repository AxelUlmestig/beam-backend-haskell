module Vector (
	Vector(..),
	add,
	mul,
	distance,
	abs
) where

import Prelude hiding (abs)
 
data Vector = Vector Double Double deriving (Show, Eq)

add :: Vector -> Vector -> Vector
add (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2);

mul :: Double -> Vector -> Vector
mul factor (Vector x y) = Vector (factor * x) (factor * y)

distance :: Vector -> Vector -> Double
distance (Vector x1 y1) (Vector x2 y2) = sqrt $ (x1 + x2) ^ 2 + (y1 + y2) ^ 2

abs :: Vector -> Double
abs (Vector x y) = sqrt $ x ^ 2 + y ^ 2
