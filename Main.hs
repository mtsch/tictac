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

-- readPair of ints from string
readPair :: String -> Maybe (Int, Int)
readPair s = readTwoInts s >>= listToMove

-- turn list with two elements to pair
listToMove :: [a] -> Maybe (a, a)
listToMove [i1, i2] = Just (i1, i2)
listToMove _        = Nothing

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
getIntPair = getSomething readPair "Not a pair of numbers!"

-- get Int from stdin
getInt :: IO Int
getInt  = getSomething readMaybeInt "Not an integer!"

-- attempt to put piece on board, report errors and retry if needed
putPieceAndReport :: Player -> Board -> IO Board
putPieceAndReport p b = do
    move <- getIntPair
    case putPiece p move b of
      Left err -> 
        putStrLn ("Invalid input: " ++ err) >> putPieceAndReport p b
      Right newBoard ->
        return newBoard

-- main loop
loop :: GameState -> IO ()
loop gs = do
    putStrLn $ showState gs
    let b = board gs
    let p = head $ queue gs
    let k = winK gs
    newBoard <- putPieceAndReport p b
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
                         (tail (queue gs) ++ [head (queue gs)])
                         ((roundNo gs) + 1)
        in
        loop newState

main :: IO ()
main = do putStrLn "Input board size:"
          boardSize <- getIntPair
          putStrLn "Input winning k:"
          winningK <- getInt
          loop $ initialState boardSize winningK
