module Main (main) where

import Lib

import qualified Bill (calculateBill)
import qualified Luhn (enterBankCard)

main :: IO ()
-- main = Bill.calculateBill
-- main = Luhn.enterBankCard
main = print(sqr 5)