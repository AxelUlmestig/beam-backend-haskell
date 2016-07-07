module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Vector

addTC = TestCase $ assertEqual "(3, 0) + (1, 1)"
	(add v1 v2) v3
	where	v1 = Vector 3 0
		v2 = Vector 1 1
		v3 = Vector 4 1
		

mulTC = TestCase $ assertEqual "2 * (2, 4)"
	(mul factor v1) v2
	where	factor = 2
		v1 = Vector 2 4
		v2 = Vector 4 8
		

distanceTC = TestCase $ assertEqual "distance (3, 0) (0, 4)"
	(distance v1 v2) expectedDist
	where	v1 = Vector 3 0
		v2 = Vector 0 4
		expectedDist = 5

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [
		TestLabel "add" addTC,
		TestLabel "mul" mulTC,
		TestLabel "distance" distanceTC
	]

main = defaultMain tests
