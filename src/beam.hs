module Beam (
	Beam(..),
	position,
	radius,
	timestamp,
	distance
) where

import qualified Vector

data Beam = Photon Vector.Vector Double Int | Beacon Beam Beam deriving (Show, Eq) 

position :: Beam -> Vector.Vector
position (Photon pos _ _) = pos
position (Beacon b1 b2) = (Vector.mul . invert) weightSum $ Vector.add (weightedPos b1) (weightedPos b2)
	where 	weightSum = getWeight b1 + getWeight b2
		getWeight = (^^2) . radius
		weightedPos b = (Vector.mul . getWeight) b $ position b
		invert = flip (^^) (-1)

radius :: Beam -> Double
radius (Photon _ rad _) = rad
radius (Beacon b1 b2) = sqrt $ (radius b1)^2 + (radius b2)^2

timestamp :: Beam -> Int
timestamp (Photon _ _ ts) = ts
timestamp (Beacon b1 b2) = min (timestamp b1) (timestamp b2)

distance :: Beam -> Beam -> Double
distance b1 b2 = Vector.distance (position b1) (position b2)
