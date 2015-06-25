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

humanPlayer :: Player
humanPlayer = Player "Human player" (\ _ -> getIntPair)

simpleRandomPlayerFun :: GameState -> IO Move
simpleRandomPlayerFun gs = do gen <- newStdGen
                             let [n1, m1] = take 2 $ randoms gen :: [Int]
                             return (n1 `mod` n, m1 `mod` m)
                               where (n, m) = (size . board) gs

-- no checking if moves are legal
simpleRandomPlayer = Player "Simple random player" randomPlayerFun
