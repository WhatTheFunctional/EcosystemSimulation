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
predatorSearch (Location Empty _ _ _) = False
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
moveCreature (newI, newJ) location@(Location creature i j worldState@(WorldState {grid = thisGrid}))
    | coordinatesAreInGrid (newI, newJ) thisGrid &&
      unsafeGet newI newJ thisGrid == Empty
        = let newGrid = unsafeSet Empty (i, j) (unsafeSet creature (newI, newJ) thisGrid)
              newLocation = Location creature newI newJ (setGrid newGrid worldState)
          in (newLocation, newLocation)
    | otherwise = (location, location)

wander :: RandomGen g => Location g -> (Location g, Location g)
wander (Location creature i j worldState@(WorldState {generator = thisGenerator,
                                                      grid = thisGrid}))
    = moveCreature (newI, newJ) (Location newCreature i j newWorldState)
        where (randomNumber, newGenerator) = randomR (0 :: Int, 3 ::Int) thisGenerator
              (newI, newJ) = neighborCoordinates randomNumber (i, j)
              newCreature = setCreatureActed True creature
              newWorldState = setGrid (unsafeSet newCreature (i, j) thisGrid) $ setGenerator newGenerator worldState

graze :: RandomGen g => Location g -> (Location g, Location g)
graze (Location creature i j worldState@(WorldState {grid = thisGrid}))
    = (newLocation, newLocation)
        where newCreature = setCreatureActed True $ decrementHunger $ decrementHunger creature
              newWorldState = setGrid (unsafeSet newCreature (i, j) thisGrid) worldState
              newLocation = Location newCreature i j newWorldState

hunt :: RandomGen g => Location g -> (Location g, Location g)
hunt (Location creature i j worldState@(WorldState {generator = thisGenerator,
                                                    grid = thisGrid}))
    | fst (prey !! randomNumber) < i = moveCreature (i - 1, j) (Location newCreature i j newWorldState)
    | fst (prey !! randomNumber) > i = moveCreature (i + 1, j) (Location newCreature i j newWorldState)
    | snd (prey !! randomNumber) < j = moveCreature (i, j - 1) (Location newCreature i j newWorldState)
    | snd (prey !! randomNumber) > j = moveCreature (i, j + 1) (Location newCreature i j newWorldState)
    | otherwise = let newLocation = Location newCreature i j newWorldState in (newLocation, newLocation)
        where prey = searchFor preySearch (getSearchDistance creature) (Location creature i j newWorldState)
              (randomNumber, newGenerator) = randomR (0 :: Int, ((length prey) - 1) ::Int) thisGenerator
              newCreature = setCreatureActed True creature
              newWorldState = setGrid (unsafeSet newCreature (i, j) thisGrid) $ setGenerator newGenerator worldState

flee :: RandomGen g => Location g -> (Location g, Location g)
flee (Location creature i j worldState@(WorldState {generator = thisGenerator,
                                                    grid = thisGrid}))
    | fst (predators !! randomNumber) < i = moveCreature (i + 1, j) (Location newCreature i j newWorldState)
    | fst (predators !! randomNumber) > i = moveCreature (i - 1, j) (Location newCreature i j newWorldState)
    | snd (predators !! randomNumber) < j = moveCreature (i, j + 1) (Location newCreature i j newWorldState)
    | snd (predators !! randomNumber) > j = moveCreature (i, j - 1) (Location newCreature i j newWorldState)
    | otherwise = let newLocation = Location newCreature i j newWorldState in (newLocation, newLocation)
        where predators = searchFor predatorSearch (getSearchDistance creature) (Location creature i j newWorldState)
              (randomNumber, newGenerator) = randomR (0 :: Int, ((length predators) - 1) ::Int) thisGenerator
              newCreature = setCreatureActed True creature
              newWorldState = setGrid (unsafeSet newCreature (i, j) thisGrid) $ setGenerator newGenerator worldState

chooseBehavior :: RandomGen g => Location g -> (Location g, Location g)
chooseBehavior location@(Location Empty _ _ _) = (location, location)
chooseBehavior location@(Location creature@(Rabbit l h Wander a) i j worldState)
    | length predators > 0 = let newLocation = Location (Rabbit l h Flee a) i j worldState in (newLocation, newLocation)
    | otherwise = (location, location)
        where predators = searchFor predatorSearch (getSearchDistance creature) location

performBehavior :: RandomGen g => (Location g) -> (Location g, Location g)
performBehavior location@(Location Empty _ _ _) = (location, location)
performBehavior location@(Location (Rabbit _ _ Wander _) _ _ _) = wander location
performBehavior location@(Location (Rabbit _ _ Graze _) _ _ _) = graze location
performBehavior location@(Location (Rabbit _ _ Flee _) _ _ _) = flee location
performBehavior location@(Location (Fox _ _ Wander _) _ _ _) = wander location
performBehavior location@(Location (Fox _ _ Hunt _) _ _ _) = hunt location
performBehavior location@(Location (Fox _ _ Flee _) _ _ _) = flee location
performBehavior location@(Location (Wolf _ _ Wander _) _ _ _) = wander location
performBehavior location@(Location (Wolf _ _ Hunt _) _ _ _) = hunt location

creatureDeath :: RandomGen g => Location g -> (Location g, Location g)
creatureDeath location@(Location creature i j worldState@(WorldState {io = thisIO,
                                                                      grid = thisGrid}))
    = (newLocation, newLocation)
        where newCreature = lifetimeDeath $ hungerDeath creature
              newIO = thisIO >>
                      putStr ((show i) ++ ", " ++ (show j) ++ ": ") >>
                      putStrLn ((show (getLifetime creature)) ++ ", " ++ (show (getHunger creature)))
              newLocation = Location newCreature i j (setIO newIO $ setGrid (unsafeSet newCreature (i, j) thisGrid) worldState)

updateCreature :: RandomGen g => (Int, Int) -> WorldState g -> WorldState g
updateCreature (i, j) worldState@(WorldState {grid = thisGrid})
    | coordinatesAreInGrid (i, j) thisGrid &&
      creature /= Empty &&
      not (creatureHasActed creature)
          = let (Location _ _ _ newWorldState)
                    = execState (state creatureDeath >>
                                 state performBehavior) (Location (incrementHunger $ incrementLifetime creature) i j worldState)
            in newWorldState
    | otherwise = worldState
        where creature = unsafeGet i j thisGrid

resetCreature :: RandomGen g => (Int, Int) -> WorldState g -> WorldState g
resetCreature (i, j) worldState@(WorldState {grid = thisGrid})
    | coordinatesAreInGrid (i, j) thisGrid &&
      creature /= Empty
          = setGrid (unsafeSet (setCreatureActed False creature) (i, j) thisGrid) worldState
    | otherwise = worldState
        where creature = unsafeGet i j thisGrid

updateWorld :: RandomGen g => WorldState g -> WorldState g
updateWorld worldState@(WorldState {iteration = thisIteration,
                                    io = thisIO,
                                    grid = thisGrid})
    = let newWorldState = setIO (thisIO >> putStrLn ("Iteration: " ++ (show thisIteration))) worldState
          coordinates = (,) <$> [1 .. (nrows thisGrid)] <*> [1 .. (ncols thisGrid)]
      in foldr resetCreature (foldr updateCreature newWorldState coordinates) coordinates

