module Main where

import System.Directory

-- This is a comment

sqr :: Double -> Double
sqr x = x * x

pythagoras :: Double -> Double -> Double
pythagoras a b =
  let a2 = sqr a
      b2 = sqr b
   in
      sqrt(a2 + b2)


-- promptForDouble :: String -> IO Double
-- promptForDouble message = do
--   {
--     -- removeFile "ImportantFile.docx";
--     putStr message;
--     input <- readLn;
--     return input;
--   }

-- main :: IO ()
-- main = do
--   {
--     side <- promptForDouble "Enter the length of the side: ";
--     base <- promptForDouble "Enter the length of the base: ";
--     putStr "Hypotenuse: ";
--     print (pythagoras side base);
--   }


promptForDouble :: String -> IO Double
promptForDouble message = do
    putStr message
    input <- readLn
    return input


main :: IO ()
main = do
    side <- promptForDouble "Enter the length of the side: "
    base <- promptForDouble "Enter the length of the base: "
    putStr "Hypotenuse: "
    let hyp = pythagoras side base
    print hyp


min :: Int -> Int -> Int
min a b = if (a < b)
            then a
            else b
