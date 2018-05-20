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
import Behaviors

simulateWorld :: RandomGen g => WorldState g -> Maybe (WorldState g)
simulateWorld worldState = Just (updateWorld worldState)

printWorld :: RandomGen g => WorldState g -> Maybe (WorldState g)
printWorld worldState@(WorldState {io = thisIO, grid = thisGrid})
    = Just (setIO (thisIO >> printGrid thisGrid) worldState)

waitWorld :: RandomGen g => WorldState g -> Maybe (WorldState g)
waitWorld worldState@(WorldState {io = thisIO})
    = Just (setIO (thisIO >> putStrLn "-----" >> hFlush stdout >> getChar >> return ()) worldState)

incrementWorld :: RandomGen g => WorldState g -> Maybe (WorldState g)
incrementWorld worldState = Just (incrementIteration worldState)

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
                -> if thisIteration > 1000
                   then get
                   else simulate)

runSimulation :: IO ()
runSimulation = let width = 30
                    height = 30
                    initialGrid = initGrid width height
                    generator = mkStdGen 126590563
                    (initialCount, newGenerator) = randomR (10 :: Int, floor ((fromIntegral (width * height)) * 0.1)) generator
                    initialCoordinates = take initialCount (shuffle' ((,) <$> [1..width] <*> [1..height]) (width * height) newGenerator)
                    initialPopulation = unfoldr (generatePopulation 70 25) (initialCoordinates, newGenerator)
                    iGrid = populateGrid initialPopulation initialGrid
                in putStrLn ("Population simulation with " ++ (show initialCount) ++ " creatures.\n") >>
                   performIO (evalState simulate (iGrid >>= makeWorld 0 (return ()) newGenerator))
