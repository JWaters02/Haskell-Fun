import Test.HUnit

import Lib

main :: IO ()
main = do
    runTestTT allTests
    return ()

allTests :: Test
allTests = TestList [
    sumListTests,
    maxListTests,
    maxLengthTests,
    sumPositivesTests,
    doubleListTests,
    sumPairsTests
    ]

sumListTests :: Test
sumListTests = TestList [
    TestCase (assertEqual "Sum empty list" 0 (sumList [])),
    TestCase (assertEqual "Sum single element list" 1 (sumList [1])),
    TestCase (assertEqual "Sum multiple element list" 10 (sumList [1, 2, 3, 4]))
    ]

maxListTests :: Test
maxListTests = TestList [
    TestCase (assertEqual "Max empty list" 0 (maxList [])),
    TestCase (assertEqual "Max single element list" 1 (maxList [1])),
    TestCase (assertEqual "Max multiple element list" 7 (maxList [1, 7, 3, 6, 5]))
    ]

maxLengthTests :: Test
maxLengthTests = TestList [
    TestCase (assertEqual "Max length empty list" 0 (maxLength [])),
    TestCase (assertEqual "Max length single element list" 1 (maxLength ["1"])),
    TestCase (assertEqual "Max length multiple element list" 5 (maxLength ["hi","world","!"]))
    ]

sumPositivesTests :: Test
sumPositivesTests = TestList [
    TestCase (assertEqual "Sum positives empty list" 0 (sumPositives [])),
    TestCase (assertEqual "Sum positives single element list" 1 (sumPositives [1])),
    TestCase (assertEqual "Sum positives multiple element list" 10 (sumPositives [1, -4, 3, 6]))
    ]

doubleListTests :: Test
doubleListTests = TestList [
    TestCase (assertEqual "Double empty list" [] (doubleList [])),
    TestCase (assertEqual "Double single element list" [2] (doubleList [1])),
    TestCase (assertEqual "Double multiple element list" [2, 4, 6, 8] (doubleList [1, 2, 3, 4]))
    ]

sumPairsTests :: Test
sumPairsTests = TestList [
    TestCase (assertEqual "Sum pairs empty list" [] (sumPairs [])),
    TestCase (assertEqual "Sum pairs single element list" [1] (sumPairs [(1,0)])),
    TestCase (assertEqual "Sum pairs multiple element list" [3, 10, 13] (sumPairs [(1,2), (7,3), (4,9)]))
    ]