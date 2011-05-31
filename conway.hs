module Conway where

import Data.List (nub, intersect, (\\))

type Cell = (Int, Int)

--basic glider
startingCells :: [Cell]
startingCells = [(1,0), (2,1), (0,2), (1,2), (2,2)]

-- add cells who have 3 alive neighbours
nextGeneration :: [Cell] -> [Cell]
nextGeneration cs = nub $ concat $ map (newAlive cs) cs

newAlive :: [Cell] -> Cell -> [Cell]
newAlive cs c = filter (isAlive cs) $ c:(neighBours c)

neighBours :: Cell -> [Cell]
neighBours (x,y) 
    = [(ax, ay) | 
        ax <- [x-1, x, x+1], 
        ay <- [y-1, y, y+1],
        ax /= x || ay /= y]

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

-- takes a list of live cells, and a coord, and 
-- returns a list of the neighbours that are currently alive
liveNeighbours :: [Cell] -> Cell -> [Cell]
liveNeighbours cs c
    = intersect cs $ neighBours c
