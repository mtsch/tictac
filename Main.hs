-- main module for generalized tic tac toe in a row
{-# OPTIONS_GHC -Wall #-}
module Main where

import GameLogic
import Helpers
import Player

-- spacer displayed in messages
spacer :: String
spacer = "============="

-- perform move made by player function
performMoveBy :: Player -> GameState -> IO Board
performMoveBy player gs = do
    move <- (fun player) gs
    case putPiece (head $ queue gs) move (board gs) of
      Left err -> 
        putStrLn ("Invalid input: " ++ err) >> performMoveBy player gs
      Right newBoard ->
        return newBoard

-- main loop - pair of players, playerX and playerO
loop :: (Player, Player) -> GameState -> IO ()
loop players gs = do
    putStrLn $ showState gs
    let currentPlayer = head $ queue gs
    newBoard <- if currentPlayer == X
                  then performMoveBy (fst players) gs
                  else performMoveBy (snd players) gs
    let k = winK gs
    case boardResult k newBoard of
      Winner w -> do putStrLn spacer
                     putStrLn $ (show w) ++ " wins!"
                     putStrLn $ show newBoard
      Tie      -> do putStrLn spacer
                     putStrLn "It's a tie!"
                     putStrLn $ show newBoard
      NotOver  ->
        let newState = GameState 
                         newBoard (winK gs)
                         (tail (queue gs) ++ [currentPlayer])
                         ((roundNo gs) + 1)
        in
        loop players newState

-- choose type of game and begin
main :: IO ()
main = do putStrLn "Input board size:"
          boardSize <- getSize
          putStrLn "Input winning k:"
          winningK <- getPositiveInt
          loop (humanPlayer, humanPlayer) $ initialState boardSize winningK
