module Main where

import Conway hiding (main)
import Test.QuickCheck 

main = do 
	quickCheck prop_neighBours_length
	quickCheck prop_isAlive_empty
	quickCheck prop_nextGeneration_empty
	
