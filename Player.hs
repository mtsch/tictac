module Player where

import GameLogic
import Helpers

-- a player is defined by it's name and the player function
data Player = Player { name :: String
                     , fun  :: (GameState -> IO Move)
                     }

humanPlayer :: Player
humanPlayer = Player "human player" (\ _ -> getIntPair)
