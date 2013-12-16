module Main (main) where

import Test.Framework
import Test.QuickCheck
import Test.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Data.List (nub)
import Conway

main = defaultMain tests

prop_nextGeneration_empty cs =
	(null cs) ==>  null (nextGeneration cs)

prop_neighBours_length c = length (neighBours c) == 8
prop_neighBours_bounds_maxx c = maxX (neighBours c) == (fst c) + 1
prop_neighBours_bounds_minx c = minX (neighBours c) == (fst c) - 1
prop_neighBours_bounds_maxy c = maxY (neighBours c) == (snd c) + 1
prop_neighBours_bounds_miny c = minY (neighBours c) == (snd c) - 1
prop_neighBours_unique c = 
	length (nub (neighBours c)) == length (neighBours c)

prop_isAlive_empty cs c  =
	(null cs) ==>  (isAlive cs c) == False

prop_max_min_X cs = (not (null cs)) ==> (maxX cs) >= (minX cs)
prop_max_min_Y cs = (not (null cs)) ==> (maxY cs) >= (minY cs)

horizontalBar = [(0,1), (1,1), (2,1)]
verticalBar = [(1,0), (1,1), (1,2)]
test_nextGeneration_hbar = (nextGeneration horizontalBar) @?= verticalBar 
test_nextGeneration_vbar = (nextGeneration verticalBar) @?= horizontalBar 

tests = 
	[
		testGroup "Basic properties"
		[ 
			testProperty "neighBours length " prop_neighBours_length,
			testProperty "neighBours bounds min x" prop_neighBours_bounds_minx,
			testProperty "neighBours bounds max x" prop_neighBours_bounds_maxx,
			testProperty "neighBours bounds min y" prop_neighBours_bounds_miny,
			testProperty "neighBours bounds max y" prop_neighBours_bounds_maxy,
			testProperty "neighBours unique" prop_neighBours_unique,
			testProperty "isAlive empty" prop_isAlive_empty,
			testProperty "nextGeneration empty" prop_nextGeneration_empty,
			testProperty "max X >= min X" prop_max_min_X,
			testProperty "max Y >= min Y" prop_max_min_Y
		],
		testGroup "Functional tests"
		[
			testCase "nextGeneration v-bar" test_nextGeneration_vbar,
			testCase "nextGeneration h-bar" test_nextGeneration_hbar
		]
	]
	
