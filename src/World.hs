--FSM World
--Copyright Laurence Emms 2018

module World (WorldState(..),
              performIO,
              wander,
              updateCreature,
              updateWorld) where

import Data.Matrix
import System.Random

import Creatures
import Grid

data WorldState g = WorldState {iteration :: Int,
                                io :: IO (),
                                generator :: g,
                                grid :: Maybe (Matrix Creature)}

performIO :: RandomGen g => WorldState g -> IO ()
performIO (WorldState {io = thisIO}) = thisIO

wander :: RandomGen g => Int -> Int -> WorldState g -> WorldState g
wander i j worldState@(WorldState {iteration = thisIteration,
                                   io = thisIO,
                                   generator = thisGenerator,
                                   grid = thisGrid})
    = case thisGrid of
      Nothing -> worldState
      Just aGrid -> let creature = unsafeGet i j aGrid
                        creatureLifetime = getLifetime creature
                    in if creature /= Empty &&
                          creatureLifetime <= thisIteration &&
                          i >= 1 && i <= nrows aGrid &&
                          j >= 1 && j <= ncols aGrid &&
                          newI >= 1 && newI <= nrows aGrid &&
                          newJ >= 1 && newJ <= ncols aGrid
                       then let targetCreature = unsafeGet newI newJ aGrid
                                newIO = thisIO >>
                                        putStr (show creature) >> putStr " (" >> putStr (show creatureLifetime) >> putStr "): " >>
                                        putStr (show i) >> putStr ", " >> putStr (show j) >> putStr " -> " >>
                                        putStr (show newI) >> putStr ", " >> putStrLn (show newJ)
                            in if targetCreature == Empty
                               then let newGrid = unsafeSet Empty (i, j) (unsafeSet (setLifetime (thisIteration + 1) creature) (newI, newJ) aGrid)
                                    in (WorldState {iteration = thisIteration,
                                                    io = newIO,
                                                    generator = newGenerator,
                                                    grid = Just newGrid})
                               else (WorldState {iteration = thisIteration,
                                                 io = thisIO,
                                                 generator = newGenerator,
                                                 grid = thisGrid})
                       else (WorldState {iteration = thisIteration,
                                         io = thisIO,
                                         generator = newGenerator,
                                         grid = thisGrid})
      where (randomNumber, newGenerator) = randomR (0 :: Int, 3 ::Int) thisGenerator
            newI = if randomNumber == 0
                   then i - 1
                   else (if randomNumber == 1
                         then i + 1
                         else i)
            newJ = if randomNumber == 2
                   then j - 1
                   else (if randomNumber == 3
                         then j + 1
                         else j)

updateCreature :: RandomGen g => WorldState g -> (Int, Int) -> WorldState g
updateCreature worldState (i, j)
    = wander i j worldState

updateWorld :: RandomGen g => WorldState g -> WorldState g
updateWorld worldState@(WorldState {iteration = thisIteration,
                                    io = thisIO,
                                    generator = thisGenerator,
                                    grid = thisGrid})
    = let newWorldState = (WorldState {iteration = thisIteration,
                              io = thisIO >> putStrLn ("Iteration: " ++ (show thisIteration)),
                              generator = thisGenerator,
                              grid = thisGrid})
      in case thisGrid of
         Nothing -> newWorldState
         Just aGrid -> foldl updateCreature newWorldState ((,) <$> [1 .. (nrows aGrid)] <*> [1 .. (ncols aGrid)])

