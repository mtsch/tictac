{-# OPTIONS_GHC -Wall #-}
module GameLogic
  ( Player (..)
  , Board  (..)
  , Move
  , Size
  , WinK
  , putOnBoard
  , legalMoves 
  , boardResult
  , showBoard
  )
where

import Data.List
import Data.List.Split (chunksOf)

data Player = X
            | O
            deriving(Show, Eq)

data Result = Winner Player | Tie | NotOver
              deriving (Show)

-- n × m board
data Board = Board { board :: [Maybe Player]
                   , size  :: Size
                   } deriving(Show, Eq)

type Move = (Int, Int)
type Size = (Int, Int)

type WinK = Int
type Index = Int

data Direction = RowLeft | RowDown | RowDiag1 | RowDiag2
                 deriving (Show)

-- convert Move to Int
fromMove :: Size -> Move -> Int
fromMove (n, _) (r, c) = r*n + c

-- convert Int to Move
toMove :: Size -> Int -> Move
toMove (n, _) i = (i `div` n, i `mod` n)

-- put element at nth place in a list
put :: a -> Int -> [a] -> [a]
put _ _ []     = []
put c 0 (_:xs) = c : xs
put c n (x:xs) = x : (put c (n-1) xs)

-- perform move on board for player p, if move is legal
putOnBoard :: Player -> Move -> Board -> Maybe Board
putOnBoard p move b = if   index >= n*m || bd !! index /= Nothing
                      then Nothing
                      else Just $ Board (put (Just p) index bd) (size b)
                        where
                          index  = fromMove (size b) move
                          (n, m) = (size b)
                          bd     = (board b)

-- return list of legal moves
legalMoves :: Board -> [Move]
legalMoves b = let aux _ []             = []
                   aux n ((Nothing):xs) = (toMove (size b) n) : aux (n+1) xs
                   aux n (_:xs)         = aux (n+1) xs
               in aux 0 (board b)

-- return list of indices where player p is located on board
findPlayerI :: Player -> Board -> [Index]
findPlayerI p = (findIndices (== Just p)) . board

-- check if elements of sorted l are contained in a sorted list
ins :: (Eq a) => [a] -> [a] -> Bool
ins [] _ = True
ins _ [] = False
ins (h1:t1) (h2:t2) | h1 == h2  = ins t1 (h2:t2)
                    | otherwise = ins (h1:t1) t2


-- generate list of numbers starting at i with step step and length k
genRowI :: WinK -> Index -> Index -> [Index]
genRowI k step i = [i, i+step .. i + (k-1) * step]

-- check if numbers are a valid row on a n×m board
-- right to left diagonals need to be reversed
validRowI :: Size -> [Index] -> Bool
validRowI (n, m) rowI = let tailMod = map (`mod` m) $ tail rowI in
                            ( all (== 0) tailMod ||
                              all (/= 0) tailMod) &&
                            all (\ x -> x < n*m && x >= 0) rowI

-- rev is used for right to left diagonals
genValidRow :: Bool -> Index -> WinK -> Size -> Index -> [Index]
genValidRow rev step k s i = if   validRowI s $ fun rowI
                             then rowI
                             else []
                               where
                                 rowI = genRowI k step i
                                 fun  = if rev then reverse else id

-- generate list of indices starting from i that form a row in a direction,
-- if such a row exists
genDir :: Direction -> WinK -> Size -> Index -> [Index]
genDir RowLeft  k s      = genValidRow False 1 k s
genDir RowDown  k (n, m) = genValidRow False m k (n, m)
genDir RowDiag1 k (n, m) = genValidRow False (m+1) k (n, m)
genDir RowDiag2 k (n, m) = genValidRow True (m-1) k (n, m)

-- generate all valid rows of length k from index i
genDirs :: WinK -> Size -> Index ->  [[Index]]
genDirs k s i = filter (\ l -> length l > 0)
                [ genDir RowLeft  k s i
                , genDir RowDown  k s i
                , genDir RowDiag1 k s i
                , genDir RowDiag2 k s i ]

-- check if list of indices contains winning row starting at index i
checkWinFromIndex :: WinK -> Size -> [Index] -> Index -> Bool
checkWinFromIndex k s indices i = length dirs > 0 &&
                                  (any id $ map (`ins` indices) dirs)
                                    where
                                      dirs = genDirs k s i

-- check if list of indices contains winning row
checkWin :: WinK -> Size -> [Index] -> Bool
checkWin k s indices = any id $ map (checkWinFromIndex k s indices) indices

-- check if player p has won
playerWon :: WinK -> Board -> Player -> Bool
playerWon k b p = checkWin k (size b) playerI
                    where
                      playerI = findPlayerI p b

-- check if game is ove == there are no more legal moves
gameIsOver :: Board -> Bool
gameIsOver b = length (legalMoves b) == 0

-- check if game is over and who won
boardResult :: WinK -> Board -> Result
boardResult k b | playerWon k b X = Winner X
                | playerWon k b O = Winner O
                | gameIsOver b    = Tie
                | otherwise       = NotOver

showSquare :: Maybe Player -> String
showSquare Nothing  = "."
showSquare (Just p) = show p

showBoard :: Board -> String
showBoard (Board l (_, m)) = unlines $ chunksOf m str
                               where str = concat $ map showSquare l 

