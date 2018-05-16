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

simulateWorld :: RandomGen g => WorldState g -> Maybe (WorldState g)
simulateWorld worldState = Just (updateWorld worldState)


printWorld :: RandomGen g => WorldState g -> Maybe (WorldState g)
printWorld (WorldState {iteration = thisIteration,
                        io = thisIO,
                        generator = thisGenerator,
                        grid = thisGrid})
    = Just (WorldState {iteration = thisIteration,
                        io = thisIO >> printGrid thisGrid,
                        generator = thisGenerator,
                        grid = thisGrid})

waitWorld :: RandomGen g => WorldState g -> Maybe (WorldState g)
waitWorld (WorldState {iteration = thisIteration,
                       io = thisIO, 
                       generator = thisGenerator,
                       grid = thisGrid})
    = Just (WorldState {iteration = thisIteration,
                        io = thisIO >> putStrLn "-----" >> hFlush stdout >> getChar >> return (),
                        generator = thisGenerator,
                        grid = thisGrid})

incrementWorld :: RandomGen g => WorldState g -> Maybe (WorldState g)
incrementWorld (WorldState {iteration = thisIteration,
                            io = thisIO, 
                            generator = thisGenerator,
                            grid = thisGrid})
    = Just (WorldState {iteration = thisIteration + 1,
                        io = thisIO,
                        generator = thisGenerator,
                        grid = thisGrid})

maybeStep :: RandomGen g => (WorldState g -> (Maybe (WorldState g))) -> State (Maybe (WorldState g)) (Maybe (WorldState g))
maybeStep updateFunction = state (\worldState -> let newWorldState = worldState >>= updateFunction --worldState has type Mabye (WorldState g)
                                                 in (newWorldState, newWorldState))

simulate :: RandomGen g => State (Maybe (WorldState g)) (Maybe (WorldState g))
simulate = maybeStep simulateWorld >>
           maybeStep printWorld >>
           maybeStep waitWorld >>
           maybeStep incrementWorld >>=
           (\worldState -> 
             case worldState of
             Nothing -> get
             Just (WorldState {iteration = thisIteration})
                -> if thisIteration > 100
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
                    iGrid = populateGrid initialPopulation initialGrid
                in putStrLn ("Population simulation with " ++ (show initialCount) ++ " creatures.\n") >>
                   performIO (evalState simulate (iGrid >>= makeWorld 0 (return ()) newGenerator))
