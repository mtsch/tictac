{-# OPTIONS_GHC -Wall #-}
module Player 
 ( Player (..)
 , humanPlayer
 , randomPlayer
 )
where

import GameState
import Helpers

import System.Random

-- a player is defined by it's name and the player function
data Player = Player { name :: String
                     , fun  :: (GameState -> IO Move)
                     }
instance Show Player where
  show p = name p

-- player input
humanPlayer :: Player
humanPlayer = Player "Human player" 
                     (\ _ -> putStrLn "Input move (row, col):" >> getIntPair)

-- no checking if moves are legal
pickRandomMove :: GameState -> IO Move
pickRandomMove gs = do gen <- newStdGen
                       let [n1, m1] = take 2 $ randoms gen :: [Int]
                       return (n1 `mod` n, m1 `mod` m)
                         where (n, m) = (size . board) gs

-- function for random player
randomPlayerFun :: GameState -> IO Move
randomPlayerFun gs = do move <- pickRandomMove gs
                        if moveIsLegal move gs 
                          then return move
                          else randomPlayerFun gs

-- 
randomPlayer :: Player
randomPlayer = Player "Random player" randomPlayerFun
