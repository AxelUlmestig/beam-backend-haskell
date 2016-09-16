module RefreshBeamsTest(
        tests
) where

import Test.Framework.Providers.HUnit
import Test.HUnit

import Vector
import Beam
import RefreshBeams

refreshBeamsEmptyTC = TestCase $ assertEqual "refreshBeams []"
        [] (refreshBeams limit [])
        where   limit = 0

refreshBeamsTC1 = TestCase $ assertEqual "refreshBeams [old, new]"
        [p1] (refreshBeams limit [p1, p2])
        where   p1 = Photon v 1 1
                p2 = Photon v 1 0
                v = Vector 0 0 
                limit = 0

refreshBeamsTC2 = TestCase $ assertEqual "refreshBeams [Beacon old new]"
        [p1] (refreshBeams limit [b])
        where   p1 = Photon v 1 1
                p2 = Photon v 1 0
                b = Beacon p1 p2
                v = Vector 0 0 
                limit = 0

refreshBeamsTC3 = TestCase $ assertEqual "refreshBeams [Beacon new (Beacon new old)]"
        [Beacon p1 p1] (refreshBeams limit [b2])
        where   p1 = Photon v 1 1
                p2 = Photon v 1 0
                b1 = Beacon p1 p2
                b2 = Beacon p1 b1
                v = Vector 0 0 
                limit = 0


tests = hUnitTestToTests $ TestList [
                TestLabel "refreshBeamsEmptyTC" refreshBeamsEmptyTC,
                TestLabel "refreshBeamsTC1" refreshBeamsTC1,
                TestLabel "refreshBeamsTC2" refreshBeamsTC2,
                TestLabel "refreshBeamsTC3" refreshBeamsTC3
        ]
