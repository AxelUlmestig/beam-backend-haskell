module ReduceBeams (
	reduceBeams
) where

import Beam

reduceBeams :: [Beam] -> [Beam]
reduceBeams [] = []
reduceBeams beams
	| reduceAll beams == beams	= beams
	| otherwise	 		= reduceBeams . reduceAll $ beams


reduceAll :: [Beam] -> [Beam]
reduceAll [] = []
reduceAll beams = (head headReduced) : (reduceAll . tail $ headReduced)
	where 	headReduced = reduceHead beams


reduceHead :: [Beam] -> [Beam]
reduceHead [] = []
reduceHead (beam:beams) = reduceHeadRec [] beam beams
	where 	reduceHeadRec :: [Beam] -> Beam -> [Beam] -> [Beam]
		reduceHeadRec mem devourer (beam:beams)
			| shouldMerge devourer beam	= reduceHeadRec mem (Beacon devourer beam) beams
			| otherwise 			= reduceHeadRec (mem ++ [beam]) devourer beams
		reduceHeadRec mem devourer [] = devourer : mem


shouldMerge :: Beam -> Beam -> Bool
shouldMerge b1 b2 = (joinedRad b1 b2) >= (groupRad b1 b2)
	where 	joinedRad b1 b2 = radius $ Beacon b1 b2
		groupRad b1 b2 = (/2) $ (radius b1) + (radius b2) + (distance b1 b2)
