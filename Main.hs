-- main module for tic tac toe three in a row
{-# OPTIONS_GHC -Wall #-}
module Main where

import GameLogic

initialState :: Board
initialState = Board ((take 9) $ repeat Nothing) (3, 3)

winRow :: WinK
winRow = 3
