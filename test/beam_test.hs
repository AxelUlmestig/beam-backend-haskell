module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Vector
import Beam

photonPosTC = TestCase $ assertEqual "pos Photon (1, 2)"
        v (position p) 
        where   p = Photon v 0 0
                v = Vector 1 2

beaconPosTC1 = TestCase $ assertEqual "pos Beacon [(0, 0), (2, 4)]"
        v (position beacon) 
        where   p1 = Photon (Vector 0 0) 1 0
                p2 = Photon (Vector 2 4) 1 0
                beacon = Beacon p1 p2
                v = Vector 1 2

beaconPosTC2 = TestCase $ assertEqual "pos Beacon [(0, 0), (0, 4)]"
        v (position beacon) 
        where   p1 = Photon (Vector 0 0) 3 0
                p2 = Photon (Vector 0 25) 4 0
                beacon = Beacon p1 p2
                v = Vector 0 16

photonRadTC = TestCase $ assertEqual "rad Photon (0, 0) 3 0"
        3 (radius p) 
        where   v = Vector 0 0
                p = Photon v 3 0

beaconRadTC = TestCase $ assertEqual "rad Beacon [3, 4]"
        5 (radius b) 
        where   v = Vector 0 0
                p1 = Photon v 3 0
                p2 = Photon v 4 0
                b = Beacon p1 p2

photonTimestampTC = TestCase $ assertEqual "timestamp Photon 7"
        ts (timestamp p) 
        where   ts = 7
                p = Photon (Vector 0 0) 0 ts

beaconTimestampTC = TestCase $ assertEqual "timestamp Beacon [3, 7]"
        tsMin (timestamp b) 
        where   tsMin = 3
                tsMax = 7
                p1 = Photon (Vector 0 0) 0 tsMin
                p2 = Photon (Vector 0 0) 0 tsMax
                b = Beacon p1 p2

beamDistanceTC = TestCase $ assertEqual "distance (3, 0) (0, 4)"
        dist (Beam.distance b1 b2) 
        where   b1 = Photon (Vector 3 0) 0 0
                b2 = Photon (Vector 0 4) 0 0
                dist = 5

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [
                TestLabel "photonPosTc" photonPosTC,
                TestLabel "beaconPosTC1" beaconPosTC1,
                TestLabel "beaconPosTC2" beaconPosTC2,
                TestLabel "photonPosTC" photonPosTC,
                TestLabel "beaconRadTC" beaconRadTC,
                TestLabel "photonTimestampTC" photonTimestampTC,
                TestLabel "beaconTimestampTC" beaconTimestampTC,
                TestLabel "beamDistanceTC" beamDistanceTC
        ]

main = defaultMain tests
