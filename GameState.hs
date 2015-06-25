{-# OPTIONS_GHC -Wall #-}
-- types and functions used for keeping track of the game
module GameState
  ( GameState (..)
  , Piece (..)
  , Board (..)
  , Result (..)
  , Move
  , Size
  , WinK
  , showState
  , initialState
  , nextPiece
  , makeMove
  , result
  , moveIsLegal
  )
where

import Data.Either

import InternalLogic

-- current state of the game
data GameState = GameState { board   :: Board    -- current board
                           , winK    :: WinK     -- k in a row needed to win
                           , queue   :: [Piece] -- queue of players
                           , roundNo :: Int      -- current round number
                           } deriving (Show)

-- show the state of the game
showState :: GameState -> String
showState gs = "=============\n" ++ (show $ roundNo gs) ++ ": "
               ++ (show . head $ queue gs) ++ "'s turn:\n" ++ (show $ board gs)

-- initial state for a game
initialState :: Size -> WinK -> GameState
initialState (n, m) k = GameState 
                          (Board ((take $ n*m) $ repeat Nothing) (n, m))
                          k [X, O] 1

-- find out which piece is next
nextPiece :: GameState -> Piece
nextPiece = head . queue

-- make move on GameState
makeMove :: Move -> GameState -> Either String GameState
makeMove move gs = case putPiece (nextPiece gs) move (board gs) of
                     Left err       -> Left err
                     Right newBoard -> Right $ GameState newBoard k q (rn + 1)
                       where
                         k  = winK gs
                         q  = (tail . queue $ gs) ++ [nextPiece gs]
                         rn = roundNo gs

-- find result of game
result :: GameState -> Result
result gs = boardResult (winK gs) (board gs)

-- check if move is legal
moveIsLegal :: Move -> GameState -> Bool
moveIsLegal move gs = isRight $ eitherMoveLegal move (board gs)




