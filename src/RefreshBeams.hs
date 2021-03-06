module RefreshBeams (
        refreshBeams
) where

import Beam
import ReduceBeams

refreshBeams :: Int -> [Beam] -> [Beam]
refreshBeams limit beams = reduceBeams $ ageFilterBeams limit beams

ageFilterBeams :: Int -> [Beam] -> [Beam]
ageFilterBeams limit beams = beams >>= ageFilterBeam limit

ageFilterBeam :: Int -> Beam -> [Beam]
ageFilterBeam limit (Photon v r ts)
        | timestamp photon > limit      = [photon]
        | otherwise                     = []
        where   photon = (Photon v r ts)
ageFilterBeam limit (Beacon b1 b2)
        | timestamp beacon > limit      = [beacon]
        | otherwise                     = ageFilterBeams limit [b1, b2]
        where   beacon = (Beacon b1 b2)

