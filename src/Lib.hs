module Lib
    ( runSimulation
    ) where

import System.IO
import Data.List
import Data.Matrix
import Control.Monad.State
import System.Random
import System.Random.Shuffle

data Creature = Empty | Rabbit | Fox | Wolf deriving (Eq, Ord)

instance Show Creature where
    show Empty = " "
    show Rabbit = "R"
    show Fox = "F"
    show Wolf = "W"

data WorldState = WorldState {iteration :: Int,
                              io :: IO (),
                              grid :: Maybe (Matrix Creature)}

performIO :: WorldState -> IO ()
performIO (WorldState {io = thisIO}) = thisIO

initGrid :: Int -> Int -> Matrix Creature
initGrid sizeI sizeJ = matrix sizeI sizeJ (\(i, j) -> Empty)

gridInsert :: Creature -> Int -> Int -> Matrix Creature -> Maybe (Matrix Creature)
gridInsert creature i j grid = safeSet creature (i, j) grid

printGrid :: Maybe (Matrix Creature) -> IO ()
printGrid Nothing = return ()
printGrid (Just grid) = putStrLn $ prettyMatrix grid

populateGrid :: [(Creature, Int, Int)] -> Maybe (Matrix Creature) -> Maybe (Matrix Creature)
populateGrid [] Nothing = Nothing
populateGrid [] (Just grid) = Just grid
populateGrid ((creature, i, j) : creatures) (Just grid) = populateGrid creatures (gridInsert creature i j grid)
populateGrid _ Nothing = Nothing

generatePopulation :: (RandomGen g) => ([(Int, Int)], g) -> Maybe ((Creature, Int, Int), ([(Int, Int)], g))
generatePopulation ([], generator) = Nothing
generatePopulation (((i, j) : coords), generator)
    | creatureIndex == 0 = Just ((Rabbit, i, j), (coords, generator1))
    | creatureIndex == 1 = Just ((Fox, i, j), (coords, generator1))
    | creatureIndex == 2 = Just ((Wolf, i, j), (coords, generator1))
      where (creatureIndex, generator1) = randomR (0 :: Int, 2 :: Int) generator


simulateStep :: State WorldState WorldState
simulateStep = state (\(WorldState {iteration = thisIteration,
                                    io = thisIO, 
                                    grid = thisGrid}) -> 
       
       let simulateIO = thisIO >> return ()
           newWorldState = WorldState {iteration = thisIteration,
                                       io = simulateIO,
                                       grid = thisGrid}
       in (newWorldState, newWorldState))

printStep :: State WorldState WorldState
printStep = state (\(WorldState {iteration = thisIteration,
                                 io = thisIO, 
                                 grid = thisGrid}) -> 
       let printIO = thisIO >> printGrid thisGrid
           newWorldState = WorldState {iteration = thisIteration,
                                       io = printIO,
                                       grid = thisGrid}
       in (newWorldState, newWorldState))

waitStep :: State WorldState WorldState
waitStep = state (\(WorldState {iteration = thisIteration,
                               io = thisIO, 
                               grid = thisGrid}) -> 
       let waitIO = thisIO >> hFlush stdout >> getChar >> return ()
           newWorldState = WorldState {iteration = thisIteration,
                                       io = waitIO,
                                       grid = thisGrid}
       in (newWorldState, newWorldState))

incrementStep :: State WorldState WorldState
incrementStep = state (\(WorldState {iteration = thisIteration,
                                     io = thisIO, 
                                     grid = thisGrid}) -> 
       let newWorldState = WorldState {iteration = thisIteration + 1,
                                       io = thisIO,
                                       grid = thisGrid}
       in (newWorldState, newWorldState))

simulate :: State WorldState WorldState
simulate = simulateStep >>
           printStep >>
           waitStep >>
           incrementStep >>= (\worldState@(WorldState {iteration = thisIteration, io = thisIO}) ->
           if thisIteration > 10
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
                   performIO (evalState simulate (WorldState {iteration = 0, io = return (), grid = iGrid}))
