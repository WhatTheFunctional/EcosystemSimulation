--FSM Creatures
--Copyright Laurence Emms 2018

module Creatures (Creature(..),
                  isPreyOf,
                  getLifetime,
                  setLifetime,
                  generatePopulation) where

import System.Random

data Creature = Empty |
                Rabbit Int |
                Fox Int |
                Wolf Int deriving (Eq, Ord)

instance Show Creature where
    show Empty = " "
    show (Rabbit _) = "R"
    show (Fox _) = "F"
    show (Wolf _) = "W"

isPreyOf :: Creature -> Creature -> Bool
Empty `isPreyOf` _ = False
_ `isPreyOf` Empty = False
(Rabbit _) `isPreyOf` (Fox _) = True
(Rabbit _) `isPreyOf` (Wolf _) = True
(Fox _) `isPreyOf` (Wolf _) = True
_ `isPreyOf` _ = False

getLifetime :: Creature -> Int
getLifetime Empty = 0
getLifetime (Rabbit x) = x
getLifetime (Fox x) = x
getLifetime (Wolf x) = x

setLifetime :: Int -> Creature -> Creature
setLifetime l Empty = Empty
setLifetime l (Rabbit x) = Rabbit l
setLifetime l (Fox x) = Fox l
setLifetime l (Wolf x) = Wolf l

generatePopulation :: RandomGen g => ([(Int, Int)], g) -> Maybe ((Creature, Int, Int), ([(Int, Int)], g))
generatePopulation ([], generator) = Nothing
generatePopulation (((i, j) : coords), generator)
    | creatureIndex == 0 = Just ((Rabbit 0, i, j), (coords, generator1))
    | creatureIndex == 1 = Just ((Fox 0, i, j), (coords, generator1))
    | creatureIndex == 2 = Just ((Wolf 0, i, j), (coords, generator1))
      where (creatureIndex, generator1) = randomR (0 :: Int, 2 :: Int) generator

