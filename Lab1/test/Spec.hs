import Test.HUnit

import Lib

import Bill (calculateBill, bill)
import Luhn (luhnDouble, checkValid)

main :: IO ()
main = do
    runTestTT allTests
    return ()

allTests :: Test
allTests = TestList [testSqrPositive, testSqrNegative, testBill, testLuhnDouble, testCheckValid]

testSqrPositive :: Test
testSqrPositive = TestCase (assertEqual "Squaring positive number" 9 (sqr 3))

testSqrNegative :: Test
testSqrNegative = TestCase (assertEqual "Squaring Negative number" 9 (sqr (-3)))

testBill :: Test
testBill = TestCase (assertEqual "Bill" 9 (bill 1 1 1))

testLuhnDouble :: Test
testLuhnDouble = TestCase (assertEqual "Luhn Double" 1 (luhnDouble 5))

testCheckValid :: Test
testCheckValid = TestCase (assertEqual "Check Valid" False (checkValid 1 2 3 4))