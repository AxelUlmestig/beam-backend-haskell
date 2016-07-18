module Vector (
        Vector(..),
        add,
        mul,
        subtract,
        distance,
        abs
) where

import Prelude hiding (abs, subtract)
 
data Vector = Vector Double Double deriving (Show, Eq)

add :: Vector -> Vector -> Vector
add (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2);

mul :: Double -> Vector -> Vector
mul factor (Vector x y) = Vector (factor * x) (factor * y)

subtract :: Vector -> Vector -> Vector
subtract v1 v2 = add v1 (neg v2)
        where neg = mul (-1)

distance :: Vector -> Vector -> Double
distance v1 v2 = abs $ subtract v1 v2

abs :: Vector -> Double
abs (Vector x y) = sqrt $ x ^ 2 + y ^ 2
