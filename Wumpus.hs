module Wumpus where

import System.IO
import Map

data Wumpus = Wumpus { wLoc :: Int, prevWLoc :: Int}

wMove :: Int -> Wumpus -> Wumpus
wMove x w = Wumpus (x) (wLoc w)

--wumpusIsAdjacent :: Player -> Map -> Wumpus -> Bool
--wumpusIsAdjacent p m w = elem (location w) (adjacentRooms p m)
