module Player where

import System.IO
import Map
import Wumpus

data Player = Player { pLoc :: Int, prevPLoc :: Int, arrows :: Int }

pMove :: Int -> Player -> Player
pMove x p = Player (x) (pLoc p) (arrows p)

wumpusIsAdjacent :: Player -> Map -> Wumpus -> Bool
wumpusIsAdjacent p m w = elem (wLoc w) $ adjacentRooms (pLoc p) m
