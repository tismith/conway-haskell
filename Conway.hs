module Conway where
import Data.List (nub, intersect)
import Control.Monad.Trans.State (State(..), runState, get, put)
import Control.Monad (forM_)

type Cell = (Int, Int)
type Board = State [Cell]

--basic glider
startingCells :: [Cell]
startingCells = [(1,0), (2,1), (0,2), (1,2), (2,2), --glider
	(5,5),(5,4),(5,6)]

nextGeneration :: Board ()
nextGeneration = do 
        cs <- get
        f <- newAlive
        put $ nub $ concatMap f cs

newAlive :: Board (Cell -> [Cell])
newAlive = do 
    f <- isAlive
    return $ \c -> filter f $ c:neighBours c

neighBours :: Cell -> [Cell]
neighBours (x,y) 
    = [(ax, ay) | 
        ax <- [x-1, x, x+1], 
        ay <- [y-1, y, y+1],
        ax /= x || ay /= y]

-- Returns a function to see if the give cell is alive next gen
isAlive :: Board (Cell -> Bool)
isAlive = do
        cs <- get
        f <- liveNeighbours
        return $ \c ->  
            let aliveCount = length $ f c in
            if aliveCount < 2 then False
            else if aliveCount == 2 then c `elem` cs
            else if aliveCount == 3 then True 
            else if aliveCount > 3 then False
            else False

-- takes a list of live cells, and a coord, and 
-- returns a list of the neighbours that are currently alive
liveNeighbours :: Board (Cell -> [Cell])
liveNeighbours = do
        cs <- get
        return $ \c -> intersect cs $ neighBours c

maxY :: [Cell] -> Int
maxX :: [Cell] -> Int
minY :: [Cell] -> Int
minX :: [Cell] -> Int
maxY = maximum . map snd
maxX = maximum . map fst
minY = minimum . map snd
minX = minimum . map fst

printCells :: [Cell] -> IO ()
printCells [] = return ()
printCells cells =  
	forM_ [(minY cells) .. (maxY cells)] (\y -> 
            forM_ [(minX cells) .. (maxX cells)] (\x -> 
                putStr (if (x,y) `elem` cells then 
                            "X" else "."))
            >> putStrLn "")

runLife :: (Show n, Enum n) => n -> [Cell] -> IO ()
runLife n cells = do
	putStrLn ("generation " ++ show n)
	getLine
	printCells cells
	runLife (succ n) $ snd $ runState nextGeneration cells


	
