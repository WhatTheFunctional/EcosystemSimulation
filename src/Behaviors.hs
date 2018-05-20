--FSM World
--Copyright Laurence Emms 2018

module Behaviors (updateCreature,
                  updateWorld) where

import Data.List
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

mateSearch :: RandomGen g => Int -> Int -> Location g -> Bool
mateSearch _ _ (Location Empty _ _ _) = False
mateSearch i j (Location (Rabbit _ _ _ _) ci cj (WorldState {grid = thisGrid}))
    | i == ci && j == cj = False
    | otherwise = case safeGet ci cj thisGrid of
                  Just (Rabbit _ _ _ _) -> True
                  _ -> False
mateSearch i j (Location (Fox _ _ _ _) ci cj (WorldState {grid = thisGrid}))
    | i == ci && j == cj = False
    | otherwise = case safeGet ci cj thisGrid of
                  Just (Fox _ _ _ _) -> True
                  _ -> False
mateSearch i j (Location (Wolf _ _ _ _) ci cj (WorldState {grid = thisGrid}))
    | i == ci && j == cj = False
    | otherwise = case safeGet ci cj thisGrid of
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

unsafeSetCreatureInLocation :: RandomGen g => Creature -> Int -> Int -> Location g -> Location g
unsafeSetCreatureInLocation creature i j (Location _ _ _ worldState)
    = Location creature i j (unsafeSetCreature creature i j worldState)

emptySpace :: Matrix Creature -> (Int, Int) -> Bool
emptySpace grid (i, j)
    = case safeGet i j grid of
      Nothing -> False
      Just Empty -> True
      Just _ -> False

reproduce :: RandomGen g => Location g -> (Location g, Location g)
reproduce location@(Location creature i j worldState@(WorldState {generator = thisGenerator,
                                                                  grid = thisGrid}))
    | null mates = (location, location)
    | gridDistance (i, j) (mates !! randomNumber) == 1
        = let (mi, mj) = (mates !! randomNumber)
              newLocation = case find (emptySpace thisGrid) (map (neighborCoordinates (i, j)) [0..3]) of
                            Nothing -> location
                            Just (ci, cj) -> unsafeSetCreatureInLocation (setState Wander $ setLifetime 0 $ setHunger 0 $ creature) ci cj location
          in (newLocation, newLocation)
    | otherwise = (location, location)
        where mates = searchFor (mateSearch i j) (getSearchDistance creature) location
              (randomNumber, newGenerator) = if null mates
                                             then randomR (0 :: Int, 3 ::Int) thisGenerator
                                             else randomR (0 :: Int, ((length mates) - 1) ::Int) thisGenerator

consume :: RandomGen g => Location g -> (Location g, Location g)
consume location@(Location creature i j worldState@(WorldState {generator = thisGenerator,
                                                                grid = thisGrid}))
    | null prey = (location, location)
    | gridDistance (i, j) (prey !! randomNumber) == 1
        = let (pi, pj) = (prey !! randomNumber)
              newLocation = unsafeSetCreatureInLocation (setHunger ((getHunger creature) - (foodValue $ unsafeGet pi pj thisGrid)) creature) i j (unsafeSetCreatureInLocation Empty pi pj location)
          in (newLocation, newLocation)
    | otherwise = (location, location)
        where prey = searchFor preySearch (getSearchDistance creature) location
              (randomNumber, newGenerator) = randomR (0 :: Int, ((length prey) - 1) ::Int) thisGenerator

wander :: RandomGen g => Location g -> (Location g, Location g)
wander location@(Location creature i j worldState@(WorldState {generator = thisGenerator}))
    | null mates = moveCreature (neighborCoordinates (i, j) randomNumber) (Location newCreature i j newWorldState)
    | fst (mates !! randomNumber) < i = moveCreature (i - 1, j) (Location newCreature i j newWorldState)
    | fst (mates !! randomNumber) > i = moveCreature (i + 1, j) (Location newCreature i j newWorldState)
    | snd (mates !! randomNumber) < j = moveCreature (i, j - 1) (Location newCreature i j newWorldState)
    | snd (mates !! randomNumber) > j = moveCreature (i, j + 1) (Location newCreature i j newWorldState)
    | otherwise = let newLocation = Location newCreature i j newWorldState in (newLocation, newLocation)
        where mates = searchFor (mateSearch i j) (getSearchDistance creature) location
              (randomNumber, newGenerator) = if null mates
                                             then randomR (0 :: Int, 3 ::Int) thisGenerator
                                             else randomR (0 :: Int, ((length mates) - 1) ::Int) thisGenerator
              newCreature = setCreatureActed True creature
              newWorldState = unsafeSetCreature newCreature i j $ setGenerator newGenerator worldState

graze :: RandomGen g => Location g -> (Location g, Location g)
graze location@(Location creature i j worldState)
    = (newLocation, newLocation)
        where newCreature = setCreatureActed True $ decrementHunger $ decrementHunger creature
              newLocation = unsafeSetCreatureInLocation newCreature i j location

hunt :: RandomGen g => Location g -> (Location g, Location g)
hunt location@(Location creature i j worldState@(WorldState {generator = thisGenerator}))
    | null prey = moveCreature (neighborCoordinates (i, j) randomNumber) (Location newCreature i j newWorldState)
    | fst (prey !! randomNumber) < i = moveCreature (i - 1, j) (Location newCreature i j newWorldState)
    | fst (prey !! randomNumber) > i = moveCreature (i + 1, j) (Location newCreature i j newWorldState)
    | snd (prey !! randomNumber) < j = moveCreature (i, j - 1) (Location newCreature i j newWorldState)
    | snd (prey !! randomNumber) > j = moveCreature (i, j + 1) (Location newCreature i j newWorldState)
    | otherwise = let newLocation = Location newCreature i j newWorldState in (newLocation, newLocation)
        where prey = searchFor preySearch (getSearchDistance creature) location
              (randomNumber, newGenerator) = if null prey
                                             then randomR (0 :: Int, 3 ::Int) thisGenerator
                                             else randomR (0 :: Int, ((length prey) - 1) ::Int) thisGenerator
              newCreature = setCreatureActed True creature
              newWorldState = unsafeSetCreature newCreature i j $ setGenerator newGenerator worldState

flee :: RandomGen g => Location g -> (Location g, Location g)
flee location@(Location creature i j worldState@(WorldState {generator = thisGenerator}))
    | null predators = moveCreature (neighborCoordinates (i, j) randomNumber) (Location newCreature i j newWorldState)
    | fst (predators !! randomNumber) < i = moveCreature (i + 1, j) (Location newCreature i j newWorldState)
    | fst (predators !! randomNumber) > i = moveCreature (i - 1, j) (Location newCreature i j newWorldState)
    | snd (predators !! randomNumber) < j = moveCreature (i, j + 1) (Location newCreature i j newWorldState)
    | snd (predators !! randomNumber) > j = moveCreature (i, j - 1) (Location newCreature i j newWorldState)
    | otherwise = let newLocation = Location newCreature i j newWorldState in (newLocation, newLocation)
        where predators = searchFor predatorSearch (getSearchDistance creature) location
              (randomNumber, newGenerator) = if null predators
                                             then randomR (0 :: Int, 3 ::Int) thisGenerator
                                             else randomR (0 :: Int, ((length predators) - 1) ::Int) thisGenerator
              newCreature = setCreatureActed True creature
              newWorldState = unsafeSetCreature newCreature i j $ setGenerator newGenerator worldState

chooseBehavior :: RandomGen g => Location g -> (Location g, Location g)
--Empty
chooseBehavior location@(Location Empty _ _ _) = (location, location)
--Rabbit
chooseBehavior location@(Location creature@(Rabbit l h Wander a) i j _)
    | length predators > 0 = let newLocation = unsafeSetCreatureInLocation (Rabbit l h Flee a) i j location
                             in (newLocation, newLocation)
    | h > 0 = let newLocation = unsafeSetCreatureInLocation (Rabbit l h Graze a) i j location
              in (newLocation, newLocation)
    | otherwise = (location, location)
        where predators = searchFor predatorSearch (getSearchDistance creature) location
chooseBehavior location@(Location creature@(Rabbit l h Graze a) i j _)
    | length predators > 0 = let newLocation = unsafeSetCreatureInLocation (Rabbit l h Flee a) i j location
                             in (newLocation, newLocation)
    | h <= 0 = let newLocation = unsafeSetCreatureInLocation (Rabbit l h Wander a) i j location
               in (newLocation, newLocation)
    | otherwise = (location, location)
        where predators = searchFor predatorSearch (getSearchDistance creature) location
chooseBehavior location@(Location creature@(Rabbit l h Flee a) i j worldState)
    | length predators == 0 = let newLocation = unsafeSetCreatureInLocation (Rabbit l h Wander a) i j location
                              in (newLocation, newLocation)
    | otherwise = (location, location)
        where predators = searchFor predatorSearch (getSearchDistance creature) location
--Fox
chooseBehavior location@(Location creature@(Fox l h Wander a) i j _)
    | length predators > 0 = let newLocation = unsafeSetCreatureInLocation (Fox l h Flee a) i j location
                             in (newLocation, newLocation)
    | h > 0 = let newLocation = unsafeSetCreatureInLocation (Fox l h Hunt a) i j location
              in (newLocation, newLocation)
    | otherwise = (location, location)
        where predators = searchFor predatorSearch (getSearchDistance creature) location
chooseBehavior location@(Location creature@(Fox l h Hunt a) i j _)
    | length predators > 0 = let newLocation = unsafeSetCreatureInLocation (Fox l h Flee a) i j location
                             in (newLocation, newLocation)
    | h <= 0 = let newLocation = unsafeSetCreatureInLocation (Fox l h Wander a) i j location
               in (newLocation, newLocation)
    | otherwise = (location, location)
        where predators = searchFor predatorSearch (getSearchDistance creature) location
chooseBehavior location@(Location creature@(Fox l h Flee a) i j worldState)
    | length predators == 0 = let newLocation = unsafeSetCreatureInLocation (Fox l h Wander a) i j location
                              in (newLocation, newLocation)
    | otherwise = (location, location)
        where predators = searchFor predatorSearch (getSearchDistance creature) location
--Wolf
chooseBehavior location@(Location creature@(Wolf l h Wander a) i j _)
    | h > 0 = let newLocation = unsafeSetCreatureInLocation (Wolf l h Hunt a) i j location
              in (newLocation, newLocation)
    | otherwise = (location, location)
chooseBehavior location@(Location creature@(Wolf l h Hunt a) i j _)
    | h <= 0 = let newLocation = unsafeSetCreatureInLocation (Wolf l h Wander a) i j location
               in (newLocation, newLocation)
    | otherwise = (location, location)

chooseBehavior location
    = (location, location)

performBehavior :: RandomGen g => (Location g) -> (Location g, Location g)
performBehavior location@(Location Empty _ _ _) = (location, location)
performBehavior location@(Location (Rabbit _ _ Wander _) _ _ _) = runState (state wander >> state reproduce) location
performBehavior location@(Location (Rabbit _ _ Graze _) _ _ _) = graze location
performBehavior location@(Location (Rabbit _ _ Flee _) _ _ _) = flee location
performBehavior location@(Location (Fox _ _ Wander _) _ _ _) = runState (state wander >> state reproduce) location
performBehavior location@(Location (Fox _ _ Hunt _) _ _ _) = runState (state hunt >> state consume) location
performBehavior location@(Location (Fox _ _ Flee _) _ _ _) = flee location
performBehavior location@(Location (Wolf _ _ Wander _) _ _ _) = runState (state wander >> state reproduce) location
performBehavior location@(Location (Wolf _ _ Hunt _) _ _ _) = runState (state hunt >> state consume) location

creatureDeath :: RandomGen g => Location g -> (Location g, Location g)
creatureDeath location@(Location creature i j worldState@(WorldState {io = thisIO}))
    = (newLocation, newLocation)
        where newCreature = lifetimeDeath $ hungerDeath creature
              newIO = thisIO >>
                      putStr ((show i) ++ ", " ++ (show j) ++ " (" ++ (show creature) ++ "): ") >>
                      putStrLn ("Lifetime: " ++ (show (getLifetime creature)) ++ " Hunger: " ++ (show (getHunger creature)) ++ " State: " ++ (show (getState creature)))
              newLocation = Location newCreature i j (setIO newIO $ unsafeSetCreature newCreature i j worldState)

updateCreature :: RandomGen g => (Int, Int) -> WorldState g -> WorldState g
updateCreature (i, j) worldState@(WorldState {grid = thisGrid})
    | coordinatesAreInGrid (i, j) thisGrid &&
      creature /= Empty &&
      not (creatureHasActed creature)
          = let (Location _ _ _ newWorldState)
                    = execState (state creatureDeath >>
                                 state chooseBehavior >>
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

