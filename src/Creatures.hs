--FSM Creatures
--Copyright Laurence Emms 2018

module Creatures (CreatureState(..),
                  Creature(..),
                  getSearchDistance,
                  getLifetime,
                  setLifetime,
                  getHunger,
                  setHunger,
                  incrementHunger,
                  decrementHunger,
                  generatePopulation) where

import System.Random

data CreatureState = Wander | Graze | Hunt | Flee deriving (Show, Eq, Ord)

data Creature = Empty |
                Rabbit Int Int CreatureState |
                Fox Int Int CreatureState |
                Wolf Int Int CreatureState deriving (Eq, Ord)

instance Show Creature where
    show Empty = " "
    show (Rabbit _ _ _) = "R"
    show (Fox _ _ _) = "F"
    show (Wolf _ _ _) = "W"

getSearchDistance :: Creature -> Int
getSearchDistance Empty = 0
getSearchDistance (Rabbit _ _ _) = 2
getSearchDistance (Fox _ _ _) = 4
getSearchDistance (Wolf _ _ _) = 3

getLifetime :: Creature -> Int
getLifetime Empty = 0
getLifetime (Rabbit x _ _) = x
getLifetime (Fox x _ _) = x
getLifetime (Wolf x _ _) = x

setLifetime :: Int -> Creature -> Creature
setLifetime l Empty = Empty
setLifetime l (Rabbit x h s) = Rabbit l h s
setLifetime l (Fox x h s) = Fox l h s
setLifetime l (Wolf x h s) = Wolf l h s

getHunger :: Creature -> Int
getHunger Empty = 0
getHunger (Rabbit _ x _) = x
getHunger (Fox _ x _) = x
getHunger (Wolf _ x _) = x

setHunger :: Int -> Creature -> Creature
setHunger h Empty = Empty
setHunger h (Rabbit l x s) = Rabbit l h s
setHunger h (Fox l x s) = Fox l h s
setHunger h (Wolf l x s) = Wolf l h s

incrementHunger :: Creature -> Creature
incrementHunger Empty = Empty
incrementHunger (Rabbit l h s) = Rabbit l (h + 1) s
incrementHunger (Fox l h s) = Fox l (h + 1) s
incrementHunger (Wolf l h s) = Wolf l (h + 1) s

decrementHunger :: Creature -> Creature
decrementHunger Empty = Empty
decrementHunger (Rabbit l h s) = Rabbit l (h + 1) s
decrementHunger (Fox l h s) = Fox l (h + 1) s
decrementHunger (Wolf l h s) = Wolf l (h + 1) s

generatePopulation :: RandomGen g => ([(Int, Int)], g) -> Maybe ((Creature, Int, Int), ([(Int, Int)], g))
generatePopulation ([], generator) = Nothing
generatePopulation (((i, j) : coords), generator)
    | creatureIndex == 0 = Just ((Rabbit 0 0 Wander, i, j), (coords, generator1))
    | creatureIndex == 1 = Just ((Fox 0 0 Wander, i, j), (coords, generator1))
    | creatureIndex == 2 = Just ((Wolf 0 0 Wander, i, j), (coords, generator1))
        where (creatureIndex, generator1) = randomR (0 :: Int, 2 :: Int) generator

