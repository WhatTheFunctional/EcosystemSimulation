--FSM World
--Copyright Laurence Emms 2018

module World (WorldState(..),
              makeWorld,
              performIO,
              updateCreature,
              updateWorld) where

import Data.Matrix
import System.Random

import Creatures
import Grid

data WorldState g = WorldState {iteration :: Int,
                                io :: IO (),
                                generator :: g,
                                grid :: Matrix Creature}

makeWorld :: RandomGen g => Int -> IO () -> g -> Matrix Creature -> Maybe (WorldState g)
makeWorld thisIteration thisIO thisGenerator thisGrid
    = Just (WorldState {iteration = thisIteration,
                        io = thisIO,
                        generator = thisGenerator,
                        grid = thisGrid})

performIO :: RandomGen g => Maybe (WorldState g) -> IO ()
performIO Nothing = return ()
performIO (Just (WorldState {io = thisIO})) = thisIO

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

updateCreature :: RandomGen g => WorldState g -> (Int, Int) -> WorldState g
updateCreature worldState@(WorldState {iteration = thisIteration,
                                       io = thisIO,
                                       generator = thisGenerator,
                                       grid = thisGrid}) (i, j)
    = let (randomNumber, newGenerator) = randomR (0 :: Int, 3 ::Int) thisGenerator
          (newI, newJ) = neighborCoordinates randomNumber (i, j)
          creature = unsafeGet i j thisGrid
          creatureLifetime = getLifetime creature
          newWorldState = WorldState {iteration = thisIteration,
                                      io = thisIO,
                                      generator = newGenerator,
                                      grid = thisGrid}
      in if creature /= Empty &&
            creatureLifetime <= thisIteration &&
            coordinatesAreInGrid (i, j) thisGrid &&
            coordinatesAreInGrid (newI, newJ) thisGrid
         then let targetCreature = unsafeGet newI newJ thisGrid
                  newIO = thisIO >>
                          putStr (show creature) >> putStr " (" >> putStr (show creatureLifetime) >> putStr "): " >>
                          putStr (show i) >> putStr ", " >> putStr (show j) >> putStr " -> " >>
                          putStr (show newI) >> putStr ", " >> putStrLn (show newJ)
              in if targetCreature == Empty
                 then let newGrid = unsafeSet Empty (i, j) (unsafeSet (setLifetime (thisIteration + 1) creature) (newI, newJ) thisGrid)
                      in (WorldState {iteration = thisIteration,
                                      io = newIO,
                                      generator = newGenerator,
                                      grid = newGrid})
                 else newWorldState
         else newWorldState

updateWorld :: RandomGen g => WorldState g -> WorldState g
updateWorld worldState@(WorldState {iteration = thisIteration,
                                    io = thisIO,
                                    generator = thisGenerator,
                                    grid = thisGrid})
    = let newWorldState = (WorldState {iteration = thisIteration,
                                       io = thisIO >> putStrLn ("Iteration: " ++ (show thisIteration)),
                                       generator = thisGenerator,
                                       grid = thisGrid})
      in foldl updateCreature newWorldState ((,) <$> [1 .. (nrows thisGrid)] <*> [1 .. (ncols thisGrid)])

