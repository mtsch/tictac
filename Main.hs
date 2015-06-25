-- main module for generalized tic tac toe in a row
{-# OPTIONS_GHC -Wall #-}
module Main where

import GameLogic

import Text.Read

data GameState = GameState { board   :: Board    -- current board
                           , winK    :: WinK     -- k in a row needed to win
                           , queue   :: [Player] -- queue of players
                           , roundNo :: Int      -- current round number
                           } deriving (Show)

initialState :: Size -> WinK -> GameState
initialState (n, m) k = GameState 
                          (Board ((take $ n*m) $ repeat Nothing) (n, m))
                          k
                          [X, O]
                          1

-- spacer displayed in messages
spacer :: String
spacer = "============="

-- show the state of the game
showState :: GameState -> String
showState gs = spacer ++ "\n" ++ (show $ roundNo gs) ++ ": " 
               ++ (show . head $ queue gs) ++ "'s turn:\n" ++ (show $ board gs)

-- read int
readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

-- read two ints from string
readTwoInts :: String -> Maybe [Int]
readTwoInts s | length wds /= 2 = Nothing
              | otherwise       = sequence $ map readMaybeInt wds
                where
                  wds = words s

-- turn list with two elements to pair
listToMove :: [a] -> Maybe (a, a)
listToMove [i1, i2] = Just (i1, i2)
listToMove _        = Nothing

-- readPair of ints from string
readIntPair :: String -> Maybe (Int, Int)
readIntPair s = readTwoInts s >>= listToMove

-- read positive int
readPositiveInt :: String -> Maybe Int
readPositiveInt s = readMaybeInt s >>= (\ i -> if i > 0 
                                               then Just i 
                                               else Nothing)

-- read valid size of board - board must be at least 2×2
readSize :: String -> Maybe Size
readSize s = readIntPair s >>= (\ (n, m) -> if n > 1 && m > 1
                                              then Just (n, m)
                                              else Nothing)

-- try to get something that can be read by readFun from stdin
getSomething :: (String -> Maybe a) -> String -> IO a
getSomething readFun errmsg = do
    input <- getLine
    case readFun input of
      Nothing -> 
        do putStrLn $ "Invalid input: " ++ errmsg
           getSomething readFun errmsg
      Just p -> 
        return p

-- get pair of Int from stdin
getIntPair :: IO (Int, Int)
getIntPair = getSomething readIntPair "Not a pair of numbers!"

-- get Int from stdin
getInt :: IO Int
getInt = getSomething readMaybeInt "Not an integer!"

-- get Int from stdin
getSize :: IO Size
getSize = getSomething readSize "Not a valid size! Must be at least 2×2!"

-- positive int only
getPositiveInt :: IO Int
getPositiveInt = getSomething readPositiveInt "Not a valid positive integer!"

-- ask human player for input
humanPlayer :: GameState -> IO Move
humanPlayer _ = getIntPair

-- perform move made by player function
performMoveBy :: (GameState -> IO Move) -> GameState -> IO Board
performMoveBy player gs = do
    move <- player gs
    case putPiece (head $ queue gs) move (board gs) of
      Left err -> 
        putStrLn ("Invalid input: " ++ err) >> performMoveBy player gs
      Right newBoard ->
        return newBoard

-- main loop - pair of players, playerX and playerO
loop :: ((GameState -> IO Move), (GameState -> IO Move)) -> 
        GameState -> IO ()
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
