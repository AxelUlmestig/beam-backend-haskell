module Beam (
	Beam(..),
	position,
	radius,
	timestamp,
	distance
) where

import qualified Vector

data Beam = Photon Vector.Vector Double Int | Beacon [Beam] deriving (Show) 

position :: Beam -> Vector.Vector
position (Photon pos _ _) = pos
position (Beacon photons) = Vector.mul (invertedLen photons) $ foldr addPos zeroVector photons 
	where 	invert = flip (^^) (-1)
		invertedLen = invert . fromIntegral . length
		zeroVector = Vector.Vector 0 0
		addPos = Vector.add . position

radius :: Beam -> Double
radius (Photon _ rad _) = rad
radius (Beacon photons) = sqrt $ foldr (addSquare . radius) 0 photons
	where	addSquare new old = old + new ^ 2

timestamp :: Beam -> Int
timestamp (Photon _ _ ts) = ts
timestamp (Beacon photons) = foldr (min . timestamp) (maxBound :: Int) photons

distance :: Beam -> Beam -> Double
distance b1 b2 = Vector.distance (position b1) (position b2)
