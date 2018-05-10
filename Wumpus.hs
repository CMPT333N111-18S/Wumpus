module Wumpus where

import System.IO
import Map

data Wumpus = Wumpus { wLoc :: Int, prevWLoc :: Int}

wMove :: Int -> Wumpus -> Wumpus
wMove x w = Wumpus (x) (wLoc w)

playerIsAdjacent :: Int -> Map -> Wumpus -> Bool
playerIsAdjacent p m w = elem p $ adjacentRooms (wLoc w) m
