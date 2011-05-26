module Conway where

type Cells = [(Int, Int)]

--basic glider
startingCells :: Cells
startingCells = [(1,0), (2,1), (2,0), (2,1), (2,2)]

-- add cells who have 3 alive neighbours
newAlive :: Cells -> Cells
newAlive = undefined

-- filter out dead cells who have < 2 or > 3 alive neighbours
filterAlive :: Cells -> Cells
filterAlive cs = filter (isAlive cs) cs

-- takes an gameboard and a live cell, and returns if it's alive next gen 
-- or not
isAlive :: Cells -> (Int, Int) -> Bool
isAlive cs c 
    | aliveCount < 2 = False
    | aliveCount == 2 = True
    | aliveCount == 3 = True
    | aliveCount > 3 = False
    | otherwise = False
    where
        aliveCount = length $ liveNeighbours cs c

liveNeighbours cells (x, y) 
    = [ (ax, ay) | 
        (ax, ay) <- cells, 
        ax == x || ax == x + 1 || ax == x - 1, 
        ay == y || ay == ay + 1 || ay == ay - 1, 
        ax /= x && ay /= y ]
    
nextGeneration :: Cells -> Cells
nextGeneration c = (newAlive c) ++ (filterAlive c)
