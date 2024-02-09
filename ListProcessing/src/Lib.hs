module Lib ( sumList, maxList, maxLength, sumPositives, doubleList, sumPairs ) where

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

maxList :: [Int] -> Int
maxList [] = 0
maxList (x:xs) = max x (maxList xs)

maxLength :: [String] -> Int
maxLength [] = 0
maxLength (x:xs) = max (length x) (maxLength xs)

sumPositives :: [Int] -> Int
sumPositives [] = 0
sumPositives (x:xs) = if x > 0 then x + sumPositives xs else sumPositives xs

doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = (2 * x) : doubleList xs

sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs ((x, y):xs) = (x + y) : sumPairs xs