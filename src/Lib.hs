--FSM Simulation
--Copyright Laurence Emms 2018

module Lib (runSimulation) where

import System.IO
import Data.List
import Data.Matrix
import Control.Monad.State
import System.Random
import System.Random.Shuffle

import Creatures
import Grid
import World

simulateStep :: RandomGen g => State (WorldState g) (WorldState g)
simulateStep = state (\worldState@(WorldState {iteration = thisIteration,
                                               io = thisIO, 
                                               generator = thisGenerator,
                                               grid = thisGrid})
                      -> let newWorldState = updateWorld worldState
                         in (newWorldState, newWorldState))

printStep :: RandomGen g => State (WorldState g) (WorldState g)
printStep = state (\(WorldState {iteration = thisIteration,
                                 io = thisIO, 
                                 generator = thisGenerator,
                                 grid = thisGrid})
                   -> let printIO = thisIO >> printGrid thisGrid
                          newWorldState = WorldState {iteration = thisIteration,
                                                      io = printIO,
                                                      generator = thisGenerator,
                                                      grid = thisGrid}
                      in (newWorldState, newWorldState))

waitStep :: RandomGen g => State (WorldState g) (WorldState g)
waitStep = state (\(WorldState {iteration = thisIteration,
                                io = thisIO, 
                                generator = thisGenerator,
                                grid = thisGrid})
                  -> let waitIO = thisIO >> putStrLn "-----" >> hFlush stdout >> getChar >> return ()
                         newWorldState = WorldState {iteration = thisIteration,
                                                     io = waitIO,
                                                     generator = thisGenerator,
                                                     grid = thisGrid}
                     in (newWorldState, newWorldState))

incrementStep :: RandomGen g => State (WorldState g) (WorldState g)
incrementStep = state (\(WorldState {iteration = thisIteration,
                                     io = thisIO, 
                                     generator = thisGenerator,
                                     grid = thisGrid})
                       -> let newWorldState = WorldState {iteration = thisIteration + 1,
                                                          io = thisIO,
                                                          generator = thisGenerator,
                                                          grid = thisGrid}
                          in (newWorldState, newWorldState))

simulate :: RandomGen g => State (WorldState g) (WorldState g)
simulate = simulateStep >>
           printStep >>
           waitStep >>
           incrementStep >>= (\worldState@(WorldState {iteration = thisIteration, io = thisIO}) ->
           if thisIteration > 100
           then get
           else simulate)

runSimulation :: IO ()
runSimulation = let width = 20
                    height = 20
                    initialGrid = initGrid width height
                    generator = mkStdGen 126590563
                    (initialCount, newGenerator) = randomR (10 :: Int, floor ((fromIntegral (width * height)) * 0.1)) generator
                    initialCoordinates = take initialCount (shuffle' ((,) <$> [1..width] <*> [1..height]) (width * height) newGenerator)
                    initialPopulation = unfoldr generatePopulation (initialCoordinates, newGenerator)
                    iGrid = populateGrid initialPopulation (Just initialGrid)
                in putStrLn ("Population simulation with " ++ (show initialCount) ++ " creatures.\n") >>
                   performIO (evalState simulate (WorldState {iteration = 0, io = return (), generator = newGenerator, grid = iGrid}))
