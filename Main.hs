module Main where
import Conway

main :: IO ()
main = runLife (0::Integer) startingCells
