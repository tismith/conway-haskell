module Conway where
import Data.List (nub, intersect)
import Control.Monad (forM_)
import Test.QuickCheck

type Cell = (Int, Int)

--basic glider
startingCells :: [Cell]
startingCells = [(1,0), (2,1), (0,2), (1,2), (2,2), --glider
	(5,5),(5,4),(5,6)]

-- add cells who have 3 alive neighBours
nextGeneration :: [Cell] -> [Cell]
nextGeneration cs = nub $ concat $ map (newAlive cs) cs
prop_nextGeneration_empty cs =
	(null cs) ==>  null (nextGeneration cs)

newAlive :: [Cell] -> Cell -> [Cell]
newAlive cs c = filter (isAlive cs) $ c:(neighBours c)

neighBours :: Cell -> [Cell]
neighBours (x,y) 
    = [(ax, ay) | 
        ax <- [x-1, x, x+1], 
        ay <- [y-1, y, y+1],
        ax /= x || ay /= y]
prop_neighBours_length c = length (neighBours c) == 8

-- takes a gameboard and cell, and returns if it's alive next gen 
-- or not
isAlive :: [Cell] -> Cell -> Bool
isAlive cs c 
    | aliveCount < 2 = False
    | aliveCount == 2 = (c `elem` cs)
    | aliveCount == 3 = True
    | aliveCount > 3 = False
    | otherwise = False
    where
        aliveCount = length $ liveNeighbours cs c
prop_isAlive_empty cs c  =
	(null cs) ==>  (isAlive cs c) == False

-- takes a list of live cells, and a coord, and 
-- returns a list of the neighbours that are currently alive
liveNeighbours :: [Cell] -> Cell -> [Cell]
liveNeighbours cs c
    = intersect cs $ neighBours c

maxY :: [Cell] -> Int
maxX :: [Cell] -> Int
minY :: [Cell] -> Int
minX :: [Cell] -> Int
maxY = maximum . (map (snd))
maxX = maximum . (map (fst))
minY = minimum . (map (snd))
minX = minimum . (map (fst))
prop_max_min_X cs = (not (null cs)) ==> (maxX cs) >= (minX cs)
prop_max_min_Y cs = (not (null cs)) ==> (maxY cs) >= (minY cs)

printCells :: [Cell] -> IO ()
printCells [] = return ()
printCells cells =  
	forM_ [(minY cells) .. (maxY cells)] (\y -> 
		forM_ [(minX cells) .. (maxX cells)] (\x -> 
			if ((x,y) `elem` cells) then 
				putStr "X" 
			else 
				putStr ".")
		>> putStrLn "")

runLife :: (Show n, Enum n) => n -> [Cell] -> IO ()
runLife n cells = do
	putStrLn ("generation " ++ show n)
	getLine
	printCells cells
	runLife (succ n) $ nextGeneration cells


	
