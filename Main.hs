module Main where

import System.IO
import Map
import Player
import Wumpus
import Data.Char
import System.Random
import System.IO.Unsafe
import System.Process

main :: IO()
main = do
  putStrLn "Welcome to Hunt the Wumpus!"
  putStrLn "---------------------------"
  putStrLn "Game Rules:"
  putStrLn "-Map consists of 20 rooms"
  putStrLn "-Each room is connected to 3 other rooms"
  putStrLn "-Player starts in room 1"
  putStrLn "-Wumpus starts in a random room that is not room 1"
  putStrLn "-Super Bats appear in 3 random rooms throughout the map"
  putStrLn "-Bottomless Pits appear in 3 random rooms through the map"
  putStrLn "-Entering a room with the Wumpus, Super Bats, or a Bottomless Pit will end the game"
  putStrLn "(Hint: Super Bats, Bottomless Pits, and the Wumpus may all be in the same room!)"
  putStrLn ""
  putStrLn "Objective:"
  putStrLn "-Shoot the Wumpus with an arrow to win the game!"
  putStrLn "-Be smart when shooting, you only have 3 arrows"
  putStrLn ""
  putStrLn "Press Enter to begin"
  let c = createMap (Map [] [] [])
  let x = generatePitsAndBats []
  let m = generateMap x c
  let p = Player 1 5 3
  let rand = unsafePerformIO (getStdRandom (randomR (2, 20)))
  let w = Wumpus rand $ adjacentRooms rand m !! 0
  putStrLn ("Wumpus is in room " ++ show (wLoc w))
  putStrLn ("Bat locations: " ++ show (bats m))
  putStrLn ("Pit locations: " ++ show (pits m))
  getLine
  game True p m w

game :: Bool -> Player -> Map -> Wumpus -> IO()
game t p m w = do
  system "cls"
  if checkForLoss p w m == 2
    then putStrLn "The Player ran out of arrows and has nothing to defend themself with! Wumpus wins."
  else if checkForLoss p w m == 3
    then putStrLn "The player was carried away by Super Bats! Game over."
  else if checkForLoss p w m == 4
    then putStrLn "The player walked into a bottomless pit, have a nice eternal fall! Game over."
  else do
  if t then do
    if checkForLoss p w m == 1
      then putStrLn "The Wumpus entered the room with the Player and ate them! Wumpus wins!"
    else do
      putStrLn "Player Turn"
      putStrLn $ "You are in room " ++ show (pLoc p)
      if wumpusIsAdjacent p m w
        then putStrLn "A foul stench is in the air..."
      else return ()
      if batsAreAdjacent (adjacentRooms (pLoc p) m) m
        then putStrLn "You hear the sound of wings fluttering nearby..."
      else return ()
      if pitIsAdjacent (adjacentRooms (pLoc p) m) m
        then putStrLn "You feel a cool draft rushing into your room..."
      else return ()
      handleMoveOrShoot p m w
    else do
      if checkForLoss p w m == 1
        then putStrLn "The Player entered the room with the Wumpus and was eaten. Wumpus wins!"
      else do
        putStrLn "Wumpus Turn"
        putStrLn $ "You are in room " ++ show (wLoc w)
        if playerIsAdjacent (pLoc p) m w
          then putStrLn "A scent of fear is in the air..."
        else return ()
        performMoveWumpus p m w

handleMoveOrShoot :: Player -> Map -> Wumpus -> IO()
handleMoveOrShoot p m w = do
  putStrLn "Would you like to move or shoot an arrow? (Enter 'move' or 'shoot'): "
  input <- getLine
  if parseMoveOrShoot input == 1
    then performMovePlayer p m w
  else if parseMoveOrShoot input == 2
    then performShoot p m w
  else do
    putStrLn "That is not an option."
    handleMoveOrShoot p m w

performShoot :: Player -> Map -> Wumpus -> IO()
performShoot p m w = do
  displayShootOptions p (adjacentRooms (pLoc p) m)
  input <- getLine
  if isNumber (head input) && elem (read input) (adjacentRooms (pLoc p) m) && checkForWin (read input) w
    then putStrLn "Player killed the Wumpus! Player wins!"
  else if isNumber(head input) && elem (read input) (adjacentRooms (pLoc p) m) && not (checkForWin (read input) w)
  then do
    putStrLn "You hear the clattering of the arrow hit the wall. The Wumpus lives..."
    putStrLn ""
    putStrLn "(Press Enter to end turn)"
    getLine
    game False (shootArrow p) m w
  else do
    putStrLn "That is not an option."
    performShoot p m w

performMovePlayer :: Player -> Map -> Wumpus -> IO()
performMovePlayer p m w = do
  displayMoveOptions (prevPLoc p) (adjacentRoomsNotPrevious (prevPLoc p) (pLoc p) m)
  input <- getLine
  if isNumber (head input) && elem (read input) (adjacentRooms (pLoc p) m)
    then game False (pMove (read input) p) m w
  else do
    putStrLn "That is not an option."
    performMovePlayer p m w

performMoveWumpus :: Player -> Map -> Wumpus -> IO()
performMoveWumpus p m w = do
  displayMoveOptions (prevWLoc w) (adjacentRoomsNotPrevious (prevWLoc w) (wLoc w) m)
  input <- getLine
  if isNumber (head input) && elem (read input) (adjacentRooms (wLoc w) m)
    then game True p m (wMove (read input) w)
  else do
    putStrLn "That is not an option."
    performMoveWumpus p m w

parseMoveOrShoot :: String -> Int
parseMoveOrShoot s  | head s == 'm' = 1
                    | head s == 's' = 2
                    | otherwise = -1

displayMoveOptions :: Int -> [Int] -> IO()
displayMoveOptions p l = do
  putStrLn $ "Do you want to go back (" ++ show p
            ++ "), to room " ++ show (l !! 0) ++ ", or to room "
            ++ show (l !! 1) ++ "? (Enter room number)"

displayShootOptions :: Player -> [Int] -> IO()
displayShootOptions p l = do
  putStrLn $ "Do you want to shoot into room " ++ show (l !! 0)
            ++ ", into room " ++ show (l !! 1) ++ ", or into room "
            ++ show (l !! 2) ++ "? (Enter room number)"

checkForWin :: Int -> Wumpus -> Bool
checkForWin x w = x == (wLoc w)

checkForLoss :: Player -> Wumpus -> Map -> Int
checkForLoss p w m  | (pLoc p) == (wLoc w)    = 1
                    | (arrows p) == 0         = 2
                    | elem (pLoc p) (bats m)  = 3
                    | elem (pLoc p) (pits m)  = 4
                    | otherwise = 0
