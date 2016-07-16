module Beam (
	Beam(..),
	position,
	radius,
	timestamp,
	distance
) where

import qualified Vector

data Beam = Photon Vector.Vector Double Int | Beacon [Beam] deriving (Show, Eq) 

position :: Beam -> Vector.Vector
position (Photon pos _ _) = pos
position (Beacon photons) = (Vector.mul . invert . sum) weights $ foldr Vector.add zeroVector weightedPos 
	where 	positions = map position photons
		weights = map weight photons
		weightedPos = zipWith Vector.mul weights positions

		weight = (^^2) . radius
		invert = (^^(-1))
		zeroVector = Vector.Vector 0 0

radius :: Beam -> Double
radius (Photon _ rad _) = rad
radius (Beacon photons) = sqrt $ foldr (addSquare . radius) 0 photons
	where	addSquare = (+) . (^2)

timestamp :: Beam -> Int
timestamp (Photon _ _ ts) = ts
timestamp (Beacon photons) = foldr (min . timestamp) (maxBound :: Int) photons

distance :: Beam -> Beam -> Double
distance b1 b2 = Vector.distance (position b1) (position b2)
