module ReduceBeams (
	reduceBeams
) where

import Beam

reduceBeams :: [Beam] -> [Beam]
reduceBeams (beam:beams)
	| once == twice		= once
	| otherwise	 	= reduceBeams twice
	where 	
		once = reduceAll (beam:beams)
		twice = reduceAll once
reduceBeams [] = []


reduceAll :: [Beam] -> [Beam]
reduceAll (beam:beams) = (head headReduced) : (reduceAll . tail $ headReduced)
	where 	headReduced = reduceHead (beam:beams)
reduceAll [] = []


reduceHead :: [Beam] -> [Beam]
reduceHead (beam:beams) = reduceHeadRec [] beam beams
	where 	reduceHeadRec :: [Beam] -> Beam -> [Beam] -> [Beam]
		reduceHeadRec mem devourer (beam:beams)
			| shouldMerge devourer beam	= reduceHeadRec mem (merge devourer beam) beams
			| otherwise 			= reduceHeadRec (mem ++ [beam]) devourer beams
		reduceHeadRec mem devourer [] = devourer : mem
reduceHead [] = []


merge :: Beam -> Beam -> Beam
merge b1 b2 = Beacon [b1, b2]


shouldMerge :: Beam -> Beam -> Bool
shouldMerge b1 b2 = (joinedRad b1 b2) >= (groupRad b1 b2)
	where 	joinedRad b1 b2 = radius $ Beacon [b1, b2]
		groupRad b1 b2 = (/2) $ (radius b1) + (radius b2) + (distance b1 b2)
