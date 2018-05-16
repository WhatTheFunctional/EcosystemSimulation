--FSM Grid
--Copyright Laurence Emms 2018

module Grid (initGrid,
             gridInsert,
             gridRemove,
             printGrid,
             populateGrid) where

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

