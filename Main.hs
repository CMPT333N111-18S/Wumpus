module Main where

import System.IO
import Map
import Player
import Wumpus
import Data.Char
import System.Random
import System.IO.Unsafe

main :: IO()
main = do
  putStrLn "Welcome to Hunt the Wumpus!"
  let m = createMap []
  let p = Player 1 5 3
  let rand = unsafePerformIO (getStdRandom (randomR (2, 20)))
  let w = Wumpus rand $ adjacentRooms rand m !! 0
  putStrLn ("Wumpus is in room " ++ show (wLoc w))
  game p m w

game :: Player -> Map -> Wumpus -> IO()
game p m w = do
  if checkForLoss p w == 1
    then putStrLn "You walked into the room with the Wumpus and got eaten! Game over."
  else do
    putStrLn $ "You are in room " ++ show (pLoc p)
    if wumpusIsAdjacent p m w
      then putStrLn "A foul stench is in the air..."
    else return ()
    displayMoveOptions p (adjacentRoomsNotPrevious (prevPLoc p) (pLoc p) m)
    input <- getLine
    if isNumber (head input) && elem (read input) (adjacentRooms (pLoc p) m)
      then game (pMove (read input) p) m w
    else do
      putStrLn "That is not an option."
      game p m w

displayMoveOptions :: Player -> [Int] -> IO()
displayMoveOptions p l = do
  putStrLn $ "Do you want to go back (" ++ show (prevPLoc p)
            ++ "), to room " ++ show (l !! 0) ++ ", or to room "
            ++ show (l !! 1) ++ "? (Enter room number)"

checkForLoss :: Player -> Wumpus -> Int
checkForLoss p w | (pLoc p) == (wLoc w) = 1
                          | otherwise = 0
