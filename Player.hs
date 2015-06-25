module Player where

import GameLogic
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
humanPlayer = Player "Human player" (\ _ -> getIntPair)

-- no checking if moves are legal
pickRandomMove :: GameState -> IO Move
pickRandomMove gs = do gen <- newStdGen
                       let [n1, m1] = take 2 $ randoms gen :: [Int]
                       return (n1 `mod` n, m1 `mod` m)
                         where (n, m) = (size . board) gs

randomPlayerFun :: GameState -> IO Move
randomPlayerFun gs = do move <- pickRandomMove gs
                        case moveIsLegal move (board gs) of
                          Left _   -> randomPlayerFun gs
                          Right () -> return move

randomPlayer :: Player
randomPlayer = Player "Random player" randomPlayerFun
