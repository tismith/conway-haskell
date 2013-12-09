module Main (main) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Conway

main = defaultMain tests

tests = 
	[
		testGroup "Basic properties"
		[ 
			testProperty "neighBours length " prop_neighBours_length,
			testProperty "isAlive empty" prop_isAlive_empty,
			testProperty "nextGeneration empty" prop_nextGeneration_empty,
			testProperty "max X >= min X" prop_max_min_X,
			testProperty "max Y >= min Y" prop_max_min_Y
		]
	]
	
