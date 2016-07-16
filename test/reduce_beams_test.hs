module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Vector
import Beam
import ReduceBeams

reduceBeamsEmptyTC = TestCase $ assertEqual "reduceBeams []"
	[] (reduceBeams [])

reduceBeamsTC1 = TestCase $ assertEqual "reduceBeams [p, p]"
	[merged] (reduceBeams [p, p])
	where 	p = Photon (Vector 0 0) 3 0
		merged = Beacon p p

reduceBeamsTC2 = TestCase $ assertEqual "reduceBeams [p1, p2]"
	beams (reduceBeams beams)
	where 	p1 = Photon (Vector 0 0) 3 0
		p2 = Photon (Vector 3 3) 3 0
		beams = [p1, p2]

reduceBeamsTC3 = TestCase $ assertEqual "reduceBeams [p1, p2, p1]"
	[merged, p2] (reduceBeams [p1, p2, p1]) 
	where 	p1 = Photon (Vector 0 0) 3 0
		p2 = Photon (Vector 3 3) 3 0
		merged = Beacon p1 p1

reduceBeamsTC4 = TestCase $ assertEqual "reduceBeams [p1, p2, p2]"
	[p1, merged] (reduceBeams [p1, p2, p2]) 
	where 	p1 = Photon (Vector 0 0) 3 0
		p2 = Photon (Vector 3 3) 3 0
		merged = Beacon p2 p2

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [
		TestLabel "reduceBeamsEmptyTC" reduceBeamsEmptyTC,
		TestLabel "reduceBeamsTC1" reduceBeamsTC1,
		TestLabel "reduceBeamsTC2" reduceBeamsTC2,
		TestLabel "reduceBeamsTC3" reduceBeamsTC3,
		TestLabel "reduceBeamsTC4" reduceBeamsTC4
	]

main = defaultMain tests
