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
  let c = createMap (Map [] [] [])
  let m = generateMap c
  let p = Player 1 5 3
  let rand = unsafePerformIO (getStdRandom (randomR (2, 20)))
  let w = Wumpus rand $ adjacentRooms rand m !! 0
  putStrLn ("Wumpus is in room " ++ show (wLoc w))
  game p m w

game :: Player -> Map -> Wumpus -> IO()
game p m w = do
  putStrLn ""
  if checkForLoss p w == 1
    then putStrLn "You walked into the room with the Wumpus and got eaten! Game over."
  else if checkForLoss p w == 2
    then putStrLn "You ran out of arrows and have nothing to defend yourself with! Game over."
  else do
    putStrLn $ "You are in room " ++ show (pLoc p)
    if wumpusIsAdjacent p m w
      then putStrLn "A foul stench is in the air..."
    else return ()
    putStrLn "Would you like to move or shoot an arrow? (Enter 'move' or 'shoot'): "
    input <- getLine
    if handleMoveOrShoot input == 1
    then do
      displayMoveOptions p (adjacentRoomsNotPrevious (prevPLoc p) (pLoc p) m)
      input <- getLine
      if isNumber (head input) && elem (read input) (adjacentRooms (pLoc p) m)
        then game (pMove (read input) p) m w
      else do
        putStrLn "That is not an option."
        game p m w
    else if handleMoveOrShoot input == 2
    then do
      displayShootOptions p (adjacentRooms (pLoc p) m)
      input <- getLine
      if isNumber (head input) && elem (read input) (adjacentRooms (pLoc p) m) && checkForWin (read input) w
        then putStrLn "You killed the Wumpus! You Win!"
      else if isNumber(head input) && elem (read input) (adjacentRooms (pLoc p) m) && not (checkForWin (read input) w)
      then do
        putStrLn "You hear the clattering of the arrow hit the wall. The Wumpus lives..."
        game (shootArrow p) m w
      else do
        putStrLn "That is not an option."
        game p m w
    else do
      putStrLn "That is not an option."
      game p m w

handleMoveOrShoot :: String -> Int
handleMoveOrShoot s | head s == 'm' = 1
                    | head s == 's' = 2
                    | otherwise = -1

displayMoveOptions :: Player -> [Int] -> IO()
displayMoveOptions p l = do
  putStrLn $ "Do you want to go back (" ++ show (prevPLoc p)
            ++ "), to room " ++ show (l !! 0) ++ ", or to room "
            ++ show (l !! 1) ++ "? (Enter room number)"

displayShootOptions :: Player -> [Int] -> IO()
displayShootOptions p l = do
  putStrLn $ "Do you want to shoot into room " ++ show (l !! 0)
            ++ ", into room " ++ show (l !! 1) ++ ", or into room "
            ++ show (l !! 2) ++ "? (Enter room number)"

checkForWin :: Int -> Wumpus -> Bool
checkForWin x w = x == (wLoc w)

checkForLoss :: Player -> Wumpus -> Int
checkForLoss p w  | (pLoc p) == (wLoc w) = 1
                  | (arrows p) == 0 = 2
                  | otherwise = 0
