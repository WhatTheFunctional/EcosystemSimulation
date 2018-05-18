--FSM World
--Copyright Laurence Emms 2018

module World (WorldState(..),
              makeWorld,
              performIO,
              updateCreature,
              updateWorld) where

import Data.Matrix
import System.Random
import Control.Monad.State

import Creatures
import Grid

data WorldState g = WorldState {iteration :: Int,
                                io :: IO (),
                                generator :: g,
                                grid :: Matrix Creature}

data Location g = Location Creature Int Int (WorldState g)

makeLocation :: RandomGen g => Creature -> WorldState g -> (Int, Int) -> Location g
makeLocation creature worldState (i, j) = Location creature i j worldState

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

predatorSearch :: RandomGen g => Location g -> Bool
predatorSearch (Location Empty i j (WorldState {grid = thisGrid})) = False
predatorSearch (Location (Rabbit _ _ _ _) i j (WorldState {grid = thisGrid}))
    = case safeGet i j thisGrid of
      Just (Fox _ _ _ _) -> True
      Just (Wolf _ _ _ _) -> True
      _ -> False
predatorSearch (Location (Fox _ _ _ _) i j (WorldState {grid = thisGrid}))
    = case safeGet i j thisGrid of
      Just (Wolf _ _ _ _) -> True
      _ -> False
predatorSearch (Location (Wolf _ _ _ _) i j (WorldState {grid = thisGrid})) = False

preySearch :: RandomGen g => Location g -> Bool
preySearch (Location Empty i j (WorldState {grid = thisGrid})) = False
preySearch (Location (Rabbit _ _ _ _) i j (WorldState {grid = thisGrid})) = False
preySearch (Location (Fox _ _ _ _) i j (WorldState {grid = thisGrid}))
    = case safeGet i j thisGrid of
      Just (Rabbit _ _ _ _) -> True
      _ -> False
preySearch (Location (Wolf _ _ _ _) i j (WorldState {grid = thisGrid}))
    = case safeGet i j thisGrid of
      Just (Rabbit _ _ _ _) -> True
      Just (Fox _ _ _ _) -> True
      _ -> False

mateSearch :: RandomGen g => Location g -> Bool
mateSearch (Location Empty i j (WorldState {grid = thisGrid})) = False
mateSearch (Location (Rabbit _ _ _ _) i j (WorldState {grid = thisGrid}))
    = case safeGet i j thisGrid of
      Just (Rabbit _ _ _ _) -> True
      _ -> False
mateSearch (Location (Fox _ _ _ _) i j (WorldState {grid = thisGrid}))
    = case safeGet i j thisGrid of
      Just (Fox _ _ _ _) -> True
      _ -> False
mateSearch (Location (Wolf _ _ _ _) i j (WorldState {grid = thisGrid}))
    = case safeGet i j thisGrid of
      Just (Wolf _ _ _ _) -> True
      _ -> False

searchFor :: RandomGen g => (Location g -> Bool) -> Int -> Location g -> [(Int, Int)]
searchFor searchFunction range (Location creature i j worldState)
    = filter (searchFunction . makeLocation creature worldState) ((,) <$> [(i - range) .. (i + range)] <*> [(j - range) .. (j + range)])

moveCreature :: RandomGen g => (Int, Int) -> Location g -> (Location g, Location g)
moveCreature (newI, newJ) location@(Location creature i j worldState@(WorldState {iteration = thisIteration,
                                                                                  io = thisIO,
                                                                                  generator = thisGenerator,
                                                                                  grid = thisGrid}))
    | coordinatesAreInGrid (newI, newJ) thisGrid &&
      unsafeGet newI newJ thisGrid == Empty
        = let newGrid = unsafeSet Empty (i, j) (unsafeSet (setLifetime (thisIteration + 1) creature) (newI, newJ) thisGrid)
              newLocation = Location creature newI newJ (WorldState {iteration = thisIteration,
                                                                     io = thisIO,
                                                                     generator = thisGenerator,
                                                                     grid = newGrid})
          in (newLocation, newLocation)
    | otherwise = (location, location)

wander :: RandomGen g => Location g -> (Location g, Location g)
wander (Location creature i j worldState@(WorldState {iteration = thisIteration,
                                                      io = thisIO,
                                                      generator = thisGenerator,
                                                      grid = thisGrid}))
    = moveCreature (newI, newJ) (Location newCreature i j newWorldState)
        where (randomNumber, newGenerator) = randomR (0 :: Int, 3 ::Int) thisGenerator
              (newI, newJ) = neighborCoordinates randomNumber (i, j)
              newWorldState = WorldState {iteration = thisIteration,
                                          io = thisIO,
                                          generator = newGenerator,
                                          grid = thisGrid}
              newCreature = setCreatureActed True $ incrementHunger creature

graze :: RandomGen g => Location g -> (Location g, Location g)
graze (Location creature i j worldState@(WorldState {iteration = thisIteration,
                                                     io = thisIO,
                                                     generator = thisGenerator,
                                                     grid = thisGrid}))
    = moveCreature (newI, newJ) (Location newCreature i j newWorldState)
        where (randomNumber, newGenerator) = randomR (0 :: Int, 3 ::Int) thisGenerator
              (newI, newJ) = neighborCoordinates randomNumber (i, j)
              newWorldState = WorldState {iteration = thisIteration,
                                          io = thisIO,
                                          generator = newGenerator,
                                          grid = thisGrid}
              newCreature = setCreatureActed True $ decrementHunger creature

hunt :: RandomGen g => Location g -> (Location g, Location g)
hunt (Location creature i j worldState@(WorldState {iteration = thisIteration,
                                                    io = thisIO,
                                                    generator = thisGenerator,
                                                    grid = thisGrid}))
    | fst (prey !! randomNumber) < i = moveCreature (i - 1, j) (Location newCreature i j newWorldState)
    | fst (prey !! randomNumber) > i = moveCreature (i + 1, j) (Location newCreature i j newWorldState)
    | snd (prey !! randomNumber) < j = moveCreature (i, j - 1) (Location newCreature i j newWorldState)
    | snd (prey !! randomNumber) > j = moveCreature (i, j + 1) (Location newCreature i j newWorldState)
    | otherwise = let newLocation = Location newCreature i j newWorldState in (newLocation, newLocation)
        where prey = searchFor preySearch (getSearchDistance creature) (Location creature i j newWorldState)
              (randomNumber, newGenerator) = randomR (0 :: Int, ((length prey) - 1) ::Int) thisGenerator
              newWorldState = WorldState {iteration = thisIteration,
                                          io = thisIO,
                                          generator = newGenerator,
                                          grid = thisGrid}
              newCreature = setCreatureActed True $ incrementHunger creature

flee :: RandomGen g => Location g -> (Location g, Location g)
flee (Location creature i j worldState@(WorldState {iteration = thisIteration,
                                                    io = thisIO,
                                                    generator = thisGenerator,
                                                    grid = thisGrid}))
    | fst (predators !! randomNumber) < i = moveCreature (i + 1, j) (Location newCreature i j newWorldState)
    | fst (predators !! randomNumber) > i = moveCreature (i - 1, j) (Location newCreature i j newWorldState)
    | snd (predators !! randomNumber) < j = moveCreature (i, j + 1) (Location newCreature i j newWorldState)
    | snd (predators !! randomNumber) > j = moveCreature (i, j - 1) (Location newCreature i j newWorldState)
    | otherwise = let newLocation = Location newCreature i j newWorldState in (newLocation, newLocation)
        where predators = searchFor predatorSearch (getSearchDistance creature) (Location creature i j newWorldState)
              (randomNumber, newGenerator) = randomR (0 :: Int, ((length predators) - 1) ::Int) thisGenerator
              newWorldState = WorldState {iteration = thisIteration,
                                          io = thisIO,
                                          generator = newGenerator,
                                          grid = thisGrid}
              newCreature = setCreatureActed True $ incrementHunger creature

chooseBehavior :: RandomGen g => Location g -> Creature
chooseBehavior (Location Empty _ _ _) = Empty
chooseBehavior location@(Location creature@(Rabbit l h Wander a) _ _ _)
    | length predators > 0 = (Rabbit l h Flee a)
    | otherwise = creature
        where predators = searchFor predatorSearch (getSearchDistance creature) location
--TODO

performBehavior :: RandomGen g => (Location g) -> (Location g, Location g)
performBehavior location@(Location Empty _ _ _) = (location, location)
performBehavior location@(Location (Rabbit _ _ _ True) _ _ _) = (location, location) --Creature has already acted this iteration
performBehavior location@(Location (Rabbit _ _ Wander _) _ _ _) = wander location
performBehavior location@(Location (Rabbit _ _ Graze _) _ _ _) = graze location
performBehavior location@(Location (Rabbit _ _ Flee _) _ _ _) = flee location
performBehavior location@(Location (Fox _ _ _ True) _ _ _) = (location, location) --Creature has already acted this iteration
performBehavior location@(Location (Fox _ _ Wander _) _ _ _) = wander location
performBehavior location@(Location (Fox _ _ Hunt _) _ _ _) = hunt location
performBehavior location@(Location (Fox _ _ Flee _) _ _ _) = flee location
performBehavior location@(Location (Wolf _ _ _ True) _ _ _) = (location, location) --Creature has already acted this iteration
performBehavior location@(Location (Wolf _ _ Wander _) _ _ _) = wander location
performBehavior location@(Location (Wolf _ _ Hunt _) _ _ _) = hunt location

creatureDeath :: RandomGen g => Location g -> (Location g, Location g)
creatureDeath location@(Location creature i j worldState@(WorldState {iteration = thisIteration,
                                                                      io = thisIO,
                                                                      generator = thisGenerator,
                                                                      grid = thisGrid}))
    = let newLocation = Location newCreature i j WorldState {iteration = thisIteration,
                                                             io = thisIO >>
                                                                  putStr ((show i) ++ ", " ++ (show j) ++ ": ") >>
                                                                  putStrLn (show (getLifetime creature)),
                                                             generator = thisGenerator,
                                                             grid = unsafeSet newCreature (i, j) thisGrid}
      in (newLocation, newLocation)
        where newCreature = lifetimeDeath $ hungerDeath creature

updateCreature :: RandomGen g => (Int, Int) -> WorldState g -> WorldState g
updateCreature (i, j) worldState@(WorldState {iteration = thisIteration,
                                              io = thisIO,
                                              generator = thisGenerator,
                                              grid = thisGrid})
    | coordinatesAreInGrid (i, j) thisGrid &&
      creature /= Empty
          = let (Location _ _ _ newWorldState)
                    = execState (state creatureDeath >>
                                 state performBehavior) (Location creature i j worldState)
            in newWorldState
    | otherwise = worldState
        where creature = unsafeGet i j thisGrid

resetCreature :: RandomGen g => (Int, Int) -> WorldState g -> WorldState g
resetCreature (i, j) worldState@(WorldState {iteration = thisIteration,
                                             io = thisIO,
                                             generator = thisGenerator,
                                             grid = thisGrid})
    | coordinatesAreInGrid (i, j) thisGrid &&
      creature /= Empty
          = WorldState {iteration = thisIteration,
                        io = thisIO,
                        generator = thisGenerator,
                        grid = unsafeSet (setCreatureActed False creature) (i, j) thisGrid}
    | otherwise = worldState
        where creature = unsafeGet i j thisGrid

updateWorld :: RandomGen g => WorldState g -> WorldState g
updateWorld worldState@(WorldState {iteration = thisIteration,
                                    io = thisIO,
                                    generator = thisGenerator,
                                    grid = thisGrid})
    = let newWorldState = WorldState {iteration = thisIteration,
                                      io = thisIO >> putStrLn ("Iteration: " ++ (show thisIteration)),
                                      generator = thisGenerator,
                                      grid = thisGrid}
          coordinates = (,) <$> [1 .. (nrows thisGrid)] <*> [1 .. (ncols thisGrid)]
      in foldr resetCreature (foldr updateCreature newWorldState coordinates) coordinates

