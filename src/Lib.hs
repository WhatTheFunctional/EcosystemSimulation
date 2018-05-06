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

wander :: (RandomGen g) => Int -> Int -> (Maybe (Matrix Creature), g) -> (Maybe (Matrix Creature), g)
wander i j (Nothing, generator) = (Nothing, generator)
wander i j (Just grid, generator)
    | direction == 0 = if (i - 1) > 1 && getElem (i - 1) j grid == Empty
                       then (safeSet (getElem i j grid) (i - 1, j) grid >>= safeSet Empty (i, j), generator1)
                       else (Just grid, generator1)
    | direction == 1 = if (i + 1) < (nrows grid) && getElem (i + 1) j grid == Empty
                       then (safeSet (getElem i j grid) (i + 1, j) grid >>= safeSet Empty (i, j), generator1)
                       else (Just grid, generator1)
    | direction == 2 = if (j - 1) > 1 && getElem i (j - 1) grid == Empty
                       then (safeSet (getElem i j grid) (i, j - 1) grid >>= safeSet Empty (i, j), generator1)
                       else (Just grid, generator1)
    | direction == 3 = if (j + 1) < (ncols grid) && getElem i (j + 1) grid == Empty
                       then (safeSet (getElem i j grid) (i, j + 1) grid >>= safeSet Empty (i, j), generator1)
                       else (Just grid, generator1)
    where (direction, generator1) = randomR (0 :: Int, 3 :: Int) generator

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

simulator :: (RandomGen g) => (Maybe (Matrix Creature), g) -> (IO (), (Maybe (Matrix Creature), g))
simulator (Nothing, generator) = (return (), (Nothing, generator))
simulator (Just grid, generator)
    = (printGrid (Just grid) >>
       hFlush stdout >>
       getChar >>
       return (),
       (Just grid, generator))

simulation :: (RandomGen g) => (Maybe (Matrix Creature), g) -> IO ()
simulation (Nothing, generator) = return ()
simulation (Just grid, generator) = fst (simulator (Just grid, generator))

runSimulation :: IO ()
runSimulation = let width = 50
                    height = 50
                    initialGrid = initGrid width height
                    generator = mkStdGen 126590563
                    (initialCount, newGenerator) = randomR (10 :: Int, width :: Int) generator
                    initialCoordinates = take initialCount (shuffle' ((,) <$> [1..width] <*> [1..height]) (width * height) newGenerator)
                    initialPopulation = unfoldr generatePopulation (initialCoordinates, newGenerator)
                in putStrLn ("Population simulation with " ++ (show initialCount) ++ " creatures.\n") >>
                   simulation (populateGrid initialPopulation (Just initialGrid), newGenerator)
