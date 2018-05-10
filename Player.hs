module Player where

import System.IO
import Map
import Wumpus

data Player = Player { pLoc :: Int, prevPLoc :: Int, arrows :: Int }

pMove :: Int -> Player -> Player
pMove x p = Player (x) (pLoc p) (arrows p)

wumpusIsAdjacent :: Player -> Map -> Wumpus -> Bool
wumpusIsAdjacent p m w = elem (wLoc w) $ adjacentRooms (pLoc p) m

batsAreAdjacent :: [Int] -> Map -> Bool
batsAreAdjacent (x:xs) m  | xs == [] = elem x (bats m)
                          | otherwise   = (elem x (bats m)) || (batsAreAdjacent xs m)

pitIsAdjacent :: [Int] -> Map -> Bool
pitIsAdjacent (x:xs) m  | xs == [] = elem x (pits m)
                        | otherwise   = (elem x (pits m)) || (pitIsAdjacent xs m)

shootArrow :: Player -> Player
shootArrow p = Player (pLoc p) (prevPLoc p) (arrows p - 1)
