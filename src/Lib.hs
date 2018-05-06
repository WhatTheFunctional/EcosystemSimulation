module Lib
    ( runSimulation
    ) where

import Data.List
import Data.Matrix
import Control.Monad.State
import System.Random
import System.Random.Shuffle

data Creature = Empty | Rabbit | Fox | Wolf

instance Show Creature where
    show Empty = " "
    show Rabbit = "R"
    show Fox = "F"
    show Wolf = "W"

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
    | creatureIndex == 0 = Just ((Rabbit, i, j), (coords, generator0))
    | creatureIndex == 1 = Just ((Fox, i, j), (coords, generator0))
    | creatureIndex == 2 = Just ((Wolf, i, j), (coords, generator0))
      where (creatureIndex, generator0) = randomR (0 :: Int, 2 :: Int) generator

runSimulation :: IO ()
runSimulation = let width = 50
                    height = 50
                    initialGrid = initGrid width height
                    generator = mkStdGen 126590563
                    (initialCount, newGenerator) = randomR (10 :: Int, width :: Int) generator
                    initialCoordinates = take initialCount (shuffle' ((,) <$> [1..width] <*> [1..height]) (width * height) newGenerator)
                    initialPopulation = unfoldr generatePopulation (initialCoordinates, newGenerator)
                in putStrLn ("Population simulation with " ++ (show initialCount) ++ " creatures.\n") >>
                   printGrid (populateGrid initialPopulation (Just initialGrid))
