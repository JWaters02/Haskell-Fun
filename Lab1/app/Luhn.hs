module Luhn (enterBankCard, luhnDouble, checkValid) where

luhnDouble :: Int -> Int
luhnDouble x = if x * 2 > 9 then x * 2 - 9 else x * 2

checkValid :: Int -> Int -> Int -> Int -> Bool
checkValid a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

enterDigit :: IO Int
enterDigit = do
    putStr "Enter a digit: "
    readLn

enterBankCard :: IO ()
enterBankCard = do
    a <- enterDigit
    b <- enterDigit
    c <- enterDigit
    d <- enterDigit
    if checkValid a b c d
        then putStrLn "Valid card"
        else putStrLn "Invalid card"