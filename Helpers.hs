-- random helper functions
module Helpers 
  ( getIntPair
  , getInt
  , getSize
  , getPositiveInt
  )
where

import Text.Read (readMaybe)

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
readSize :: String -> Maybe (Int, Int)
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
getSize :: IO (Int, Int)
getSize = getSomething readSize "Not a valid size! Must be at least 2×2!"

-- positive int only
getPositiveInt :: IO Int
getPositiveInt = getSomething readPositiveInt "Not a valid positive integer!"

