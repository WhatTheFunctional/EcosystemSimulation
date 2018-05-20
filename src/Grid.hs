--FSM Grid
--Copyright Laurence Emms 2018

module Grid (initGrid,
             gridInsert,
             gridRemove,
             printGrid,
             populateGrid,
             neighborCoordinates,
             coordinatesAreInGrid) where

import Creatures
import Data.Matrix

initGrid :: Int -> Int -> Matrix Creature
initGrid sizeI sizeJ = matrix sizeI sizeJ (\(i, j) -> Empty)

gridInsert :: Creature -> Int -> Int -> Matrix Creature -> Maybe (Matrix Creature)
gridInsert creature i j grid = safeSet creature (i, j) grid

gridRemove :: Int -> Int -> Matrix Creature -> Maybe (Matrix Creature)
gridRemove i j grid = safeSet Empty (i, j) grid

populateGrid :: [(Creature, Int, Int)] -> Matrix Creature -> Maybe (Matrix Creature)
populateGrid [] grid = Just grid
populateGrid ((creature, i, j) : creatures) grid = gridInsert creature i j grid >>=
                                                   populateGrid creatures

printGrid :: Matrix Creature -> IO ()
printGrid grid = putStrLn $ prettyMatrix grid

neighborCoordinates :: Int -> (Int, Int) -> (Int, Int)
neighborCoordinates x (i, j)
    | x == 0 = (i - 1, j)
    | x == 1 = (i + 1, j)
    | x == 2 = (i, j - 1)
    | x == 3 = (i, j + 1)
    | otherwise = (i, j)

coordinatesAreInGrid :: (Int, Int) -> Matrix Creature -> Bool
coordinatesAreInGrid (i, j) grid
    = i >= 1 && i <= nrows grid &&
      j >= 1 && j <= ncols grid
