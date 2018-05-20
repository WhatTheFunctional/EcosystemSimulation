--FSM Creatures
--Copyright Laurence Emms 2018

module Creatures (CreatureState(..),
                  Creature(..),
                  getSearchDistance,
                  getLifetime,
                  setLifetime,
                  incrementLifetime,
                  getHunger,
                  setHunger,
                  incrementHunger,
                  decrementHunger,
                  getState,
                  setState,
                  creatureHasActed,
                  setCreatureActed,
                  hungerDeath,
                  lifetimeDeath,
                  generatePopulation) where

import System.Random

data CreatureState = Wander | Graze | Hunt | Flee deriving (Show, Eq, Ord)

data Creature = Empty |
                Rabbit Int Int CreatureState Bool |
                Fox Int Int CreatureState Bool |
                Wolf Int Int CreatureState Bool deriving (Eq, Ord)

instance Show Creature where
    show Empty = " "
    show (Rabbit _ _ _ _) = "R"
    show (Fox _ _ _ _) = "F"
    show (Wolf _ _ _ _) = "W"

getSearchDistance :: Creature -> Int
getSearchDistance Empty = 0
getSearchDistance (Rabbit _ _ _ _) = 2
getSearchDistance (Fox _ _ _ _) = 4
getSearchDistance (Wolf _ _ _ _) = 3

getLifetime :: Creature -> Int
getLifetime Empty = 0
getLifetime (Rabbit l _ _ _) = l
getLifetime (Fox l _ _ _) = l
getLifetime (Wolf l _ _ _) = l

setLifetime :: Int -> Creature -> Creature
setLifetime l Empty = Empty
setLifetime l (Rabbit _ h s a) = Rabbit l h s a
setLifetime l (Fox _ h s a) = Fox l h s a
setLifetime l (Wolf _ h s a) = Wolf l h s a

incrementLifetime :: Creature -> Creature
incrementLifetime Empty = Empty
incrementLifetime (Rabbit l h s a) = Rabbit (l + 1) h s a
incrementLifetime (Fox l h s a) = Fox (l + 1) h s a
incrementLifetime (Wolf l h s a) = Wolf (l + 1) h s a

getHunger :: Creature -> Int
getHunger Empty = 0
getHunger (Rabbit _ h _ _) = h
getHunger (Fox _ h _ _) = h
getHunger (Wolf _ h _ _) = h

setHunger :: Int -> Creature -> Creature
setHunger h Empty = Empty
setHunger h (Rabbit l _ s a) = Rabbit l h s a
setHunger h (Fox l _ s a) = Fox l h s a
setHunger h (Wolf l _ s a) = Wolf l h s a

incrementHunger :: Creature -> Creature
incrementHunger Empty = Empty
incrementHunger (Rabbit l h s a) = Rabbit l (h + 1) s a
incrementHunger (Fox l h s a) = Fox l (h + 1) s a
incrementHunger (Wolf l h s a) = Wolf l (h + 1) s a

decrementHunger :: Creature -> Creature
decrementHunger Empty = Empty
decrementHunger (Rabbit l h s a) = Rabbit l (h + 1) s a
decrementHunger (Fox l h s a) = Fox l (h + 1) s a
decrementHunger (Wolf l h s a) = Wolf l (h + 1) s a

getState :: Creature -> Maybe CreatureState
getState Empty = Nothing
getState (Rabbit _ _ s _) = Just s
getState (Fox _ _ s _) = Just s
getState (Wolf _ _ s _) = Just s

setState :: CreatureState -> Creature -> Creature
setState s Empty = Empty
setState s (Rabbit l h _ a) = (Rabbit l h s a)
setState s (Fox l h _ a) = (Fox l h s a)
setState s (Wolf l h _ a) = (Wolf l h s a)

creatureHasActed :: Creature -> Bool
creatureHasActed Empty = False
creatureHasActed (Rabbit _ _ _ a) = a
creatureHasActed (Fox _ _ _ a) = a
creatureHasActed (Wolf _ _ _ a) = a

setCreatureActed :: Bool -> Creature -> Creature
setCreatureActed _ Empty = Empty
setCreatureActed a (Rabbit l h s _) = Rabbit l h s a
setCreatureActed a (Fox l h s _) = Fox l h s a
setCreatureActed a (Wolf l h s _) = Wolf l h s a

hungerDeath :: Creature -> Creature
hungerDeath Empty = Empty
hungerDeath creature@(Rabbit _ h _ _)
    | h > 50 = Empty
    | otherwise = creature
hungerDeath creature@(Fox _ h _ _)
    | h > 40 = Empty
    | otherwise = creature
hungerDeath creature@(Wolf _ h _ _)
    | h > 30 = Empty
    | otherwise = creature

lifetimeDeath :: Creature -> Creature
lifetimeDeath Empty = Empty
lifetimeDeath creature@(Rabbit l _ _ _)
    | l > 80 = Empty
    | otherwise = creature
lifetimeDeath creature@(Fox l _ _ _)
    | l > 100 = Empty
    | otherwise = creature
lifetimeDeath creature@(Wolf l _ _ _)
    | l > 135 = Empty
    | otherwise = creature

generatePopulation :: RandomGen g => ([(Int, Int)], g) -> Maybe ((Creature, Int, Int), ([(Int, Int)], g))
generatePopulation ([], generator) = Nothing
generatePopulation (((i, j) : coords), generator)
    | creatureIndex == 0 = Just ((Rabbit 0 0 Wander False, i, j), (coords, generator1))
    | creatureIndex == 1 = Just ((Fox 0 0 Wander False, i, j), (coords, generator1))
    | creatureIndex == 2 = Just ((Wolf 0 0 Wander False, i, j), (coords, generator1))
        where (creatureIndex, generator1) = randomR (0 :: Int, 2 :: Int) generator

