--FSM World
--Copyright Laurence Emms 2018

module Behaviors (updateCreature,
                  updateWorld) where

import Data.Matrix
import System.Random
import Control.Monad.State

import Creatures
import Grid
import World

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

unsafeSetCreature :: RandomGen g => Creature -> Int -> Int -> WorldState g -> WorldState g
unsafeSetCreature creature i j worldState@(WorldState {grid = thisGrid})
    = setGrid (unsafeSet creature (i, j) thisGrid) worldState

wander :: RandomGen g => Location g -> (Location g, Location g)
wander (Location creature i j worldState@(WorldState {generator = thisGenerator}))
    = moveCreature (newI, newJ) (Location newCreature i j newWorldState)
        where (randomNumber, newGenerator) = randomR (0 :: Int, 3 ::Int) thisGenerator
              (newI, newJ) = neighborCoordinates randomNumber (i, j)
              newCreature = setCreatureActed True creature
              newWorldState = unsafeSetCreature newCreature i j $ setGenerator newGenerator worldState

graze :: RandomGen g => Location g -> (Location g, Location g)
graze (Location creature i j worldState)
    = (newLocation, newLocation)
        where newCreature = setCreatureActed True $ decrementHunger $ decrementHunger creature
              newWorldState = unsafeSetCreature newCreature i j worldState
              newLocation = Location newCreature i j newWorldState

hunt :: RandomGen g => Location g -> (Location g, Location g)
hunt (Location creature i j worldState@(WorldState {generator = thisGenerator}))
    | fst (prey !! randomNumber) < i = moveCreature (i - 1, j) (Location newCreature i j newWorldState)
    | fst (prey !! randomNumber) > i = moveCreature (i + 1, j) (Location newCreature i j newWorldState)
    | snd (prey !! randomNumber) < j = moveCreature (i, j - 1) (Location newCreature i j newWorldState)
    | snd (prey !! randomNumber) > j = moveCreature (i, j + 1) (Location newCreature i j newWorldState)
    | otherwise = let newLocation = Location newCreature i j newWorldState in (newLocation, newLocation)
        where prey = searchFor preySearch (getSearchDistance creature) (Location creature i j newWorldState)
              (randomNumber, newGenerator) = randomR (0 :: Int, ((length prey) - 1) ::Int) thisGenerator
              newCreature = setCreatureActed True creature
              newWorldState = unsafeSetCreature newCreature i j $ setGenerator newGenerator worldState

flee :: RandomGen g => Location g -> (Location g, Location g)
flee (Location creature i j worldState@(WorldState {generator = thisGenerator}))
    | fst (predators !! randomNumber) < i = moveCreature (i + 1, j) (Location newCreature i j newWorldState)
    | fst (predators !! randomNumber) > i = moveCreature (i - 1, j) (Location newCreature i j newWorldState)
    | snd (predators !! randomNumber) < j = moveCreature (i, j + 1) (Location newCreature i j newWorldState)
    | snd (predators !! randomNumber) > j = moveCreature (i, j - 1) (Location newCreature i j newWorldState)
    | otherwise = let newLocation = Location newCreature i j newWorldState in (newLocation, newLocation)
        where predators = searchFor predatorSearch (getSearchDistance creature) (Location creature i j newWorldState)
              (randomNumber, newGenerator) = randomR (0 :: Int, ((length predators) - 1) ::Int) thisGenerator
              newCreature = setCreatureActed True creature
              newWorldState = unsafeSetCreature newCreature i j $ setGenerator newGenerator worldState

chooseBehavior :: RandomGen g => Location g -> (Location g, Location g)
chooseBehavior location@(Location Empty _ _ _) = (location, location)
chooseBehavior location@(Location creature@(Rabbit l h Wander a) i j worldState)
    | length predators > 0
        = let newCreature = Rabbit l h Flee a
              newLocation = Location newCreature i j (unsafeSetCreature newCreature i j worldState)
          in (newLocation, newLocation)
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
creatureDeath location@(Location creature i j worldState@(WorldState {io = thisIO}))
    = (newLocation, newLocation)
        where newCreature = lifetimeDeath $ hungerDeath creature
              newIO = thisIO >>
                      putStr ((show i) ++ ", " ++ (show j) ++ ": ") >>
                      putStrLn ("Lifetime: " ++ (show (getLifetime creature)) ++ " Hunger: " ++ (show (getHunger creature)) ++ " State: " ++ (show (getState creature)))
              newLocation = Location newCreature i j (setIO newIO $ unsafeSetCreature newCreature i j worldState)

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
          = unsafeSetCreature (setCreatureActed False creature) i j worldState
    | otherwise = worldState
        where creature = unsafeGet i j thisGrid

updateWorld :: RandomGen g => WorldState g -> WorldState g
updateWorld worldState@(WorldState {iteration = thisIteration,
                                    io = thisIO,
                                    grid = thisGrid})
    = foldr resetCreature (foldr updateCreature newWorldState coordinates) coordinates
        where newWorldState = setIO (thisIO >> putStrLn ("Iteration: " ++ (show thisIteration))) worldState
              coordinates = (,) <$> [1 .. (nrows thisGrid)] <*> [1 .. (ncols thisGrid)]

