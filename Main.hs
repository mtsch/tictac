-- main module for generalized tic tac toe in a row
{-# OPTIONS_GHC -Wall #-}
module Main where

import GameState
import Helpers
import Player

-- spacer displayed in messages
spacer :: String
spacer = "============="

-- perform move made by player function
performMoveBy :: Player -> GameState -> IO GameState
performMoveBy player gs = do
    move <- (fun player) gs
    case makeMove move gs of
      Left err -> 
        putStrLn ("Invalid input: " ++ err) >> performMoveBy player gs
      Right newState ->
        return newState

-- main loop - pair of players, playerX and playerO
loop :: (Player, Player) -> GameState -> IO ()
loop players gs = do
    putStrLn $ showState gs
    let currentPlayer = head $ queue gs
    newState <- if currentPlayer == X
                  then performMoveBy (fst players) gs
                  else performMoveBy (snd players) gs
    case result newState of
      Winner w -> do putStrLn spacer
                     putStrLn $ (show w) ++ " wins!"
                     putStrLn . show $ board newState
      Tie      -> do putStrLn spacer
                     putStrLn "It's a tie!"
                     putStrLn . show $ board newState
      NotOver  -> loop players newState

-- choose type of game and begin
main :: IO ()
main = do putStrLn "Input board size:"
          boardSize <- getSize
          putStrLn "Input winning k:"
          winningK <- getPositiveInt
          loop (humanPlayer, randomPlayer) $ initialState boardSize winningK
