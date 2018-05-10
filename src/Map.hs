module Map where

import Data.List
import System.Random
import System.IO.Unsafe

data Room = Room { number :: Int, conn :: [Int] }
data Map = Map { rooms :: [Room], pits :: [Int], bats :: [Int] }

adjacentRooms :: Int -> Map -> [Int]
adjacentRooms x m = conn $ (rooms m) !! (x-1)

adjacentRoomsNotPrevious :: Int -> Int -> Map -> [Int]
adjacentRoomsNotPrevious prev curr m = delete prev $ adjacentRooms curr m

createMap :: Map -> Map
createMap m = Map ([Room 1 [5,8,2]]
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
  ++ [Room 20 [16,19,13]]) [] []

generatePitsAndBats :: Map -> Map
generatePitsAndBats m = do
  let r = (rooms m)
  let rand1 = unsafePerformIO (getStdRandom (randomR (2, 7)))
  let rand2 = unsafePerformIO (getStdRandom (randomR (8, 14)))
  let rand3 = unsafePerformIO (getStdRandom (randomR (15, 20)))
  let rand4 = unsafePerformIO (getStdRandom (randomR (2, 7)))
  let rand5 = unsafePerformIO (getStdRandom (randomR (8, 14)))
  let rand6 = unsafePerformIO (getStdRandom (randomR (15, 20)))
  Map r [rand1,rand2,rand3] [rand4,rand5,rand6]
