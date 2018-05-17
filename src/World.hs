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

predatorSearch :: WorldState g -> Creature -> (Int, Int) -> Bool
predatorSearch (WorldState {grid = thisGrid}) Empty (i, j) = False
predatorSearch (WorldState {grid = thisGrid}) (Rabbit _ _ _) (i, j)
    = case safeGet i j thisGrid of
      Just (Fox _ _ _) -> True
      Just (Wolf _ _ _) -> True
      _ -> False
predatorSearch (WorldState {grid = thisGrid}) (Fox _ _ _) (i, j)
    = case safeGet i j thisGrid of
      Just (Wolf _ _ _) -> True
      _ -> False
predatorSearch (WorldState {grid = thisGrid}) (Wolf _ _ _) (i, j) = False

preySearch :: WorldState g -> Creature -> (Int, Int) -> Bool
preySearch (WorldState {grid = thisGrid}) Empty (i, j) = False
preySearch (WorldState {grid = thisGrid}) (Rabbit _ _ _) (i, j) = False
preySearch (WorldState {grid = thisGrid}) (Fox _ _ _) (i, j)
    = case safeGet i j thisGrid of
      Just (Rabbit _ _ _) -> True
      _ -> False
preySearch (WorldState {grid = thisGrid}) (Wolf _ _ _) (i, j)
    = case safeGet i j thisGrid of
      Just (Rabbit _ _ _) -> True
      Just (Fox _ _ _) -> True
      _ -> False

mateSearch :: WorldState g -> Creature -> (Int, Int) -> Bool
mateSearch (WorldState {grid = thisGrid}) Empty (i, j) = False
mateSearch (WorldState {grid = thisGrid}) (Rabbit _ _ _) (i, j)
    = case safeGet i j thisGrid of
      Just (Rabbit _ _ _) -> True
      _ -> False
mateSearch (WorldState {grid = thisGrid}) (Fox _ _ _) (i, j)
    = case safeGet i j thisGrid of
      Just (Fox _ _ _) -> True
      _ -> False
mateSearch (WorldState {grid = thisGrid}) (Wolf _ _ _) (i, j)
    = case safeGet i j thisGrid of
      Just (Wolf _ _ _) -> True
      _ -> False

searchFor :: (WorldState g -> Creature -> (Int, Int) -> Bool) -> WorldState g -> Int -> Creature -> (Int, Int) -> [(Int, Int)]
searchFor searchFunction worldState range creature (i, j) 
    = filter (searchFunction worldState creature) ((,) <$> [(i - range) .. (i + range)] <*> [(j - range) .. (j + range)])

moveCreature :: RandomGen g => (Int, Int) -> (Int, Int) -> Creature -> WorldState g -> WorldState g
moveCreature (i, j) (newI, newJ) creature worldState@(WorldState {iteration = thisIteration,
                                                                  io = thisIO,
                                                                  generator = thisGenerator,
                                                                  grid = thisGrid})
    | coordinatesAreInGrid (newI, newJ) thisGrid &&
      targetCreature == Empty
        = let newGrid = unsafeSet Empty (i, j) (unsafeSet (setLifetime (thisIteration + 1) creature) (newI, newJ) thisGrid)
          in (WorldState {iteration = thisIteration,
                          io = thisIO,
                          generator = thisGenerator,
                          grid = newGrid})
    | otherwise = worldState
        where targetCreature = unsafeGet newI newJ thisGrid

wander :: RandomGen g => WorldState g -> Creature -> (Int, Int) -> WorldState g
wander worldState@(WorldState {iteration = thisIteration,
                               io = thisIO,
                               generator = thisGenerator,
                               grid = thisGrid}) creature (i, j)
    = moveCreature (i, j) (newI, newJ) (incrementHunger creature) newWorldState
        where (randomNumber, newGenerator) = randomR (0 :: Int, 3 ::Int) thisGenerator
              (newI, newJ) = neighborCoordinates randomNumber (i, j)
              newWorldState = WorldState {iteration = thisIteration,
                                          io = thisIO,
                                          generator = newGenerator,
                                          grid = thisGrid}

graze :: RandomGen g => WorldState g -> Creature -> (Int, Int) -> WorldState g
graze worldState@(WorldState {iteration = thisIteration,
                              io = thisIO,
                              generator = thisGenerator,
                              grid = thisGrid}) creature (i, j)
    = moveCreature (i, j) (newI, newJ) (decrementHunger creature) newWorldState
        where (randomNumber, newGenerator) = randomR (0 :: Int, 3 ::Int) thisGenerator
              (newI, newJ) = neighborCoordinates randomNumber (i, j)
              newWorldState = WorldState {iteration = thisIteration,
                                          io = thisIO,
                                          generator = newGenerator,
                                          grid = thisGrid}

hunt :: RandomGen g => WorldState g -> Creature -> (Int, Int) -> WorldState g
hunt worldState@(WorldState {iteration = thisIteration,
                             io = thisIO,
                             generator = thisGenerator,
                             grid = thisGrid}) creature (i, j)
    | fst (prey !! randomNumber) < i = moveCreature (i, j) (i - 1, j) (incrementHunger creature) newWorldState 
    | fst (prey !! randomNumber) > i = moveCreature (i, j) (i + 1, j) (incrementHunger creature) newWorldState
    | snd (prey !! randomNumber) < j = moveCreature (i, j) (i, j - 1) (incrementHunger creature) newWorldState
    | snd (prey !! randomNumber) > j = moveCreature (i, j) (i, j + 1) (incrementHunger creature) newWorldState
    | otherwise = newWorldState
        where prey = searchFor preySearch newWorldState (getSearchDistance creature) creature (i, j)
              (randomNumber, newGenerator) = randomR (0 :: Int, ((length prey) - 1) ::Int) thisGenerator
              newWorldState = WorldState {iteration = thisIteration,
                                          io = thisIO,
                                          generator = newGenerator,
                                          grid = thisGrid}

flee :: RandomGen g => WorldState g -> Creature -> (Int, Int) -> WorldState g
flee worldState@(WorldState {iteration = thisIteration,
                             io = thisIO,
                             generator = thisGenerator,
                             grid = thisGrid}) creature (i, j)
    | fst (predators !! randomNumber) < i = moveCreature (i, j) (i + 1, j) (incrementHunger creature) newWorldState
    | fst (predators !! randomNumber) > i = moveCreature (i, j) (i - 1, j) (incrementHunger creature) newWorldState
    | snd (predators !! randomNumber) < j = moveCreature (i, j) (i, j + 1) (incrementHunger creature) newWorldState
    | snd (predators !! randomNumber) > j = moveCreature (i, j) (i, j - 1) (incrementHunger creature) newWorldState
    | otherwise = newWorldState
        where predators = searchFor predatorSearch newWorldState (getSearchDistance creature) creature (i, j)
              (randomNumber, newGenerator) = randomR (0 :: Int, ((length predators) - 1) ::Int) thisGenerator
              newWorldState = WorldState {iteration = thisIteration,
                                          io = thisIO,
                                          generator = newGenerator,
                                          grid = thisGrid}

performBehavior :: RandomGen g => WorldState g -> Creature -> (Int, Int) -> WorldState g
performBehavior worldState Empty (i, j) = worldState
performBehavior worldState creature@(Rabbit _ _ Wander) (i, j) = wander worldState creature (i, j)
performBehavior worldState creature@(Rabbit _ _ Graze) (i, j) = graze worldState creature (i, j)
performBehavior worldState creature@(Rabbit _ _ Flee) (i, j) = flee worldState creature (i, j)
performBehavior worldState creature@(Fox _ _ Wander) (i, j) = wander worldState creature (i, j)
performBehavior worldState creature@(Fox _ _ Hunt) (i, j) = hunt worldState creature (i, j)
performBehavior worldState creature@(Fox _ _ Flee) (i, j) = flee worldState creature (i, j)
performBehavior worldState creature@(Wolf _ _ Wander) (i, j) = wander worldState creature (i, j)
performBehavior worldState creature@(Wolf _ _ Hunt) (i, j) = hunt worldState creature (i, j)

updateCreature :: RandomGen g => WorldState g -> (Int, Int) -> WorldState g
updateCreature worldState@(WorldState {iteration = thisIteration,
                                       io = thisIO,
                                       generator = thisGenerator,
                                       grid = thisGrid}) (i, j)
    | coordinatesAreInGrid (i, j) thisGrid &&
      creature /= Empty &&
      creatureLifetime <= thisIteration
          = performBehavior worldState creature (i, j)
    | otherwise = worldState
        where creature = unsafeGet i j thisGrid
              creatureLifetime = getLifetime creature

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

