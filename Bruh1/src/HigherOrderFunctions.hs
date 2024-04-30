module HigherOrderFunctions where

import Prelude hiding (map,filter,zipWith)

import Data.Char

-- stringToUpper :: [Char] -> [Char]
-- stringToUpper [] = []
-- stringToUpper (char:rest) = [toUpper char] ++ stringToUpper rest

-- stringToLower :: [Char] -> [Char]
-- stringToLower [] = []
-- stringToLower (char:rest) = [toLower char] ++ stringToLower rest

-- stringToSameCase :: Bool -> [Char] -> [Char]
-- stringToSameCase goUpper [] = []
-- stringToSameCase goUpper (char:rest) =
--   [if goUpper then toUpper char else toLower char] ++ stringToSameCase goUpper rest

-- stringToASCIIcodes :: [Char] -> [Int]
-- stringToASCIIcodes [] = []
-- stringToASCIIcodes (char:rest) = [ord char] ++ stringToASCIIcodes rest

-- mapString :: (Char -> t) -> [Char] -> [t]
-- mapString f [] = []
-- mapString f (char:rest) = [f char] ++ mapString f rest

map :: (a -> t) -> [a] -> [t]
map f [] = []
map f (x:rest) = [f x] ++ map f rest


filter :: (a -> Bool) -> [a] -> [a]
filter keep [] = []
filter keep (x:rest) =
  let newRest = filter keep rest
   in if keep x
        then [x] ++ newRest
        else newRest

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] [] = []
zipWith f (a1:aRest) (b1:bRest) =
  [f a1 b1] ++ zipWith f aRest bRest


-- sqr :: Int -> Int
-- sqr i = i * i
