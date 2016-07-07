module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Vector
import Beam

photonPosTC = TestCase $ assertEqual "pos Photon (1, 2)"
	(position p) v
	where	p = Photon v 0 0
		v = Vector 1 2

beaconPosTC = TestCase $ assertEqual "pos Beacon [(0, 0), (2, 4)]"
	(position beacon) v
	where	p1 = Photon (Vector 0 0) 0 0
		p2 = Photon (Vector 2 4) 0 0
		beacon = Beacon [p1, p2]
		v = Vector 1 2

photonRadTC = TestCase $ assertEqual "rad Photon (0, 0) 3 0"
	(radius p) 3
	where 	v = Vector 0 0
		p = Photon v 3 0

beaconRadTC = TestCase $ assertEqual "rad Beacon [3, 4]"
	(radius b) 5
	where 	v = Vector 0 0
		p1 = Photon v 3 0
		p2 = Photon v 4 0
		b = Beacon [p1, p2]

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [
		TestLabel "photonPosTc" photonPosTC,
		TestLabel "beaconPosTC" beaconPosTC,
		TestLabel "photonPosTC" photonPosTC,
		TestLabel "beaconRadTC" beaconRadTC
	]

main = defaultMain tests
