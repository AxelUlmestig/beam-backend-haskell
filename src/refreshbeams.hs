module RefreshBeams (
	refreshBeams
) where

import Beam
import ReduceBeams

refreshBeams :: Int -> [Beam] -> [Beam]
refreshBeams limit beams = reduceBeams $ ageFilterBeams limit beams

ageFilterBeams :: Int -> [Beam] -> [Beam]
ageFilterBeams limit beams = foldr ((++) . ageFilterBeam limit) [] beams

ageFilterBeam :: Int -> Beam -> [Beam]
ageFilterBeam limit (Photon v r ts) 
	| timestamp photon > limit 	= [photon]
	| otherwise 			= []
	where 	photon = (Photon v r ts)
ageFilterBeam limit (Beacon beams)
	| timestamp beacon > limit 	= [beacon]
	| otherwise			= ageFilterBeams limit beams
	where 	beacon = (Beacon beams)

