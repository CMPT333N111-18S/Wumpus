module Map where

import Data.List

data Room = Room { number :: Int, conn :: [Int] }

type Map = [Room]

adjacentRooms :: Int -> Map -> [Int]
adjacentRooms x m = conn $ m !! (x-1)

adjacentRoomsNotPrevious :: Int -> Int -> Map -> [Int]
adjacentRoomsNotPrevious prev curr m = delete prev $ adjacentRooms curr m

createMap :: Map -> Map
createMap [] = [Room 1 [5,8,2]]
  ++ [Room 2 [1,10,3]]
  ++ [Room 3 [4,12,2]]
  ++ [Room 4 [14,3,5]]
  ++ [Room 5 [4,6,1]]
  ++ [Room 6 [5,7,15]]
  ++ [Room 7 [8,6,17]]
  ++ [Room 8 [7,9,1]]
  ++ [Room 9 [10,8,18]]
  ++ [Room 10 [9,11,2]]
  ++ [Room 11 [10,12,19]]
  ++ [Room 12 [3,11,13]]
  ++ [Room 13 [12,14,20]]
  ++ [Room 14 [15,13,4]]
  ++ [Room 15 [14,16,6]]
  ++ [Room 16 [15,17,20]]
  ++ [Room 17 [16,18,7]]
  ++ [Room 18 [9,19,17]]
  ++ [Room 19 [18,20,11]]
  ++ [Room 20 [16,19,13]]
