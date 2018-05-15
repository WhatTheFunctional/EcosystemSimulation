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

initGrid :: Int -> Int -> Matrix Creature
initGrid sizeI sizeJ = matrix sizeI sizeJ (\(i, j) -> Empty)

gridInsert :: Creature -> Int -> Int -> Matrix Creature -> Maybe (Matrix Creature)
gridInsert creature i j grid = safeSet creature (i, j) grid

gridRemove :: Int -> Int -> Matrix Creature -> Maybe (Matrix Creature)
gridRemove i j grid = safeSet Empty (i, j) grid

printGrid :: Maybe (Matrix Creature) -> IO ()
printGrid Nothing = return ()
printGrid (Just grid) = putStrLn $ prettyMatrix grid

populateGrid :: [(Creature, Int, Int)] -> Maybe (Matrix Creature) -> Maybe (Matrix Creature)
populateGrid [] Nothing = Nothing
populateGrid [] (Just grid) = Just grid
populateGrid ((creature, i, j) : creatures) (Just grid) = populateGrid creatures (gridInsert creature i j grid)
populateGrid _ Nothing = Nothing

generatePopulation :: RandomGen g => ([(Int, Int)], g) -> Maybe ((Creature, Int, Int), ([(Int, Int)], g))
generatePopulation ([], generator) = Nothing
generatePopulation (((i, j) : coords), generator)
    | creatureIndex == 0 = Just ((Rabbit, i, j), (coords, generator1))
    | creatureIndex == 1 = Just ((Fox, i, j), (coords, generator1))
    | creatureIndex == 2 = Just ((Wolf, i, j), (coords, generator1))
      where (creatureIndex, generator1) = randomR (0 :: Int, 2 :: Int) generator


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
      Just aGrid -> if newI >= 1 && newI < nrows aGrid &&
                       newJ >= 1 && newJ < ncols aGrid
                    then if unsafeGet newI newJ aGrid == Empty
                         then let newGrid = unsafeSet Empty (i, j) (unsafeSet (unsafeGet i j aGrid) (newI, newJ) aGrid)
                              in (WorldState {iteration = thisIteration,
                                              io = thisIO,
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
            newI = if randomNumber == 0 then i - 1 else (if randomNumber == 1 then i + 1 else i)
            newJ = if randomNumber == 2 then j - 1 else (if randomNumber == 3 then j + 1 else j)

simulateStep :: RandomGen g => State (WorldState g) (WorldState g)
simulateStep = state (\(WorldState {iteration = thisIteration,
                                    io = thisIO, 
                                    generator = thisGenerator,
                                    grid = thisGrid}) -> 
       let simulateIO = thisIO >> return ()
           newWorldState = WorldState {iteration = thisIteration,
                                       io = simulateIO,
                                       generator = thisGenerator,
                                       grid = thisGrid}
       in (newWorldState, newWorldState))

printStep :: RandomGen g => State (WorldState g) (WorldState g)
printStep = state (\(WorldState {iteration = thisIteration,
                                 io = thisIO, 
                                 generator = thisGenerator,
                                 grid = thisGrid}) -> 
       let printIO = thisIO >> printGrid thisGrid
           newWorldState = WorldState {iteration = thisIteration,
                                       io = printIO,
                                       generator = thisGenerator,
                                       grid = thisGrid}
       in (newWorldState, newWorldState))

waitStep :: RandomGen g => State (WorldState g) (WorldState g)
waitStep = state (\(WorldState {iteration = thisIteration,
                                io = thisIO, 
                                generator = thisGenerator,
                                grid = thisGrid}) -> 
       let waitIO = thisIO >> hFlush stdout >> getChar >> return ()
           newWorldState = WorldState {iteration = thisIteration,
                                       io = waitIO,
                                       generator = thisGenerator,
                                       grid = thisGrid}
       in (newWorldState, newWorldState))

incrementStep :: RandomGen g => State (WorldState g) (WorldState g)
incrementStep = state (\(WorldState {iteration = thisIteration,
                                     io = thisIO, 
                                     generator = thisGenerator,
                                     grid = thisGrid}) -> 
       let newWorldState = WorldState {iteration = thisIteration + 1,
                                       io = thisIO,
                                       generator = thisGenerator,
                                       grid = thisGrid}
       in (newWorldState, newWorldState))

simulate :: RandomGen g => State (WorldState g) (WorldState g)
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
                   performIO (evalState simulate (WorldState {iteration = 0, io = return (), generator = newGenerator, grid = iGrid}))
