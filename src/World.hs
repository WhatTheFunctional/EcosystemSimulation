--FSM World
--Copyright Laurence Emms 2018

module World (WorldState(..),
              setIteration,
              getIteration,
              incrementIteration,
              setIO,
              getIO,
              setGenerator,
              getGenerator,
              setGrid,
              getGrid,
              makeWorld,
              performIO,
              Location(..),
              makeLocation) where

import Data.Matrix
import System.Random
import Control.Monad.State

import Creatures
import Grid

data WorldState g = WorldState {iteration :: Int,
                                io :: IO (),
                                generator :: g,
                                grid :: Matrix Creature}

setIteration :: RandomGen g => Int -> WorldState g -> WorldState g
setIteration newIteration (WorldState {iteration = _,
                                       io = thisIO,
                                       generator = thisGenerator,
                                       grid = thisGrid})
    = WorldState {iteration = newIteration,
                  io = thisIO,
                  generator = thisGenerator,
                  grid = thisGrid}

getIteration :: RandomGen g => WorldState g -> Int
getIteration (WorldState {iteration = thisIteration}) = thisIteration

incrementIteration :: RandomGen g => WorldState g -> WorldState g
incrementIteration (WorldState {iteration = thisIteration,
                                       io = thisIO,
                                       generator = thisGenerator,
                                       grid = thisGrid})
    = WorldState {iteration = thisIteration + 1,
                  io = thisIO,
                  generator = thisGenerator,
                  grid = thisGrid}


setIO :: RandomGen g => IO () -> WorldState g -> WorldState g
setIO newIO (WorldState {iteration = thisIteration,
                         io = _,
                         generator = thisGenerator,
                         grid = thisGrid})
    = WorldState {iteration = thisIteration,
                  io = newIO,
                  generator = thisGenerator,
                  grid = thisGrid}

getIO :: RandomGen g => WorldState g -> IO ()
getIO (WorldState {io = thisIO}) = thisIO

setGenerator :: RandomGen g => g -> WorldState g -> WorldState g
setGenerator newGenerator (WorldState {iteration = thisIteration,
                                       io = thisIO,
                                       generator = _,
                                       grid = thisGrid})
    = WorldState {iteration = thisIteration,
                  io = thisIO,
                  generator = newGenerator,
                  grid = thisGrid}

getGenerator :: RandomGen g => WorldState g -> g
getGenerator (WorldState {generator = thisGenerator}) = thisGenerator

setGrid :: RandomGen g => Matrix Creature -> WorldState g -> WorldState g
setGrid newGrid (WorldState {iteration = thisIteration,
                             io = thisIO,
                             generator = thisGenerator,
                             grid = _})
    = WorldState {iteration = thisIteration,
                  io = thisIO,
                  generator = thisGenerator,
                  grid = newGrid}

getGrid :: RandomGen g => WorldState g -> Matrix Creature
getGrid (WorldState {grid = thisGrid}) = thisGrid

makeWorld :: RandomGen g => Int -> IO () -> g -> Matrix Creature -> Maybe (WorldState g)
makeWorld thisIteration thisIO thisGenerator thisGrid
    = Just (WorldState {iteration = thisIteration,
                        io = thisIO,
                        generator = thisGenerator,
                        grid = thisGrid})

performIO :: RandomGen g => Maybe (WorldState g) -> IO ()
performIO Nothing = return ()
performIO (Just (WorldState {io = thisIO})) = thisIO

data Location g = Location Creature Int Int (WorldState g)

makeLocation :: RandomGen g => Creature -> WorldState g -> (Int, Int) -> Location g
makeLocation creature worldState (i, j) = Location creature i j worldState

