import Test.QuickCheck       -- requires "QuickCheck" package
import Test.Tasty            -- requires "tasty" package
import Test.Tasty.QuickCheck -- requires "tasty-quickcheck" package
import Test.Tasty.HUnit      -- requires "HUnit" and "tasty-hunit" packages

-------------------------------------------------------------

-- QuickCheck Tests


prop_reverse_inverse :: [Int] -> Property
prop_reverse_inverse list =
   property (reverse (reverse list) == list)

prop_reverse_preservesLength :: [Int] -> Property
prop_reverse_preservesLength list =
   property (length (reverse list) == length list)

prop_reverse_headLast :: [Int] -> Property
prop_reverse_headLast list =
   property ( not (null list) ==>
              head list == last (reverse list)
            )

-------------------------------------------------------------

-- Grouping tests into test suites

reverse_qcheck_tests :: TestTree
reverse_qcheck_tests = testGroup "QuickCheck tests for 'reverse'"
  [ testProperty "Reverse is an inverse operation" prop_reverse_inverse,
    testProperty "reverse preserves length" prop_reverse_preservesLength,
    testProperty "the head before reversing is the last after reversing" prop_reverse_headLast
  ]

-------------------------------------------------------------

-- HUnit Tests

test_reverse_shortList :: Assertion
test_reverse_shortList =
    assertEqual "" [5,4,3,2,1] (reverse [1,2,3,4,5])

test_reverse_singletonList :: Assertion
test_reverse_singletonList =
    assertEqual "" [8] (reverse [8])

test_reverse_emptyList :: Assertion
test_reverse_emptyList =
    let emptyList :: [Int]
        emptyList = []
     in
        assertEqual "" emptyList (reverse emptyList)

-------------------------------------------------------------

-- Grouping tests into test suites

reverse_hunit_tests :: TestTree
reverse_hunit_tests = testGroup "HUnit tests for 'reverse'"
 [ testCase "reverse short list"     test_reverse_shortList,
   testCase "reverse singleton list" test_reverse_singletonList,
   testCase "reverse empty list"     test_reverse_emptyList
 ]

-------------------------------------------------------------

-- If desired, you can have groups of tests within groups.  E.g.

-- extra_tests :: TestTree
-- extra_tests = undefined

-- more_tests :: TestTree
-- more_tests = undefined

all_tests :: TestTree
all_tests = testGroup "All Tests" [reverse_qcheck_tests, reverse_hunit_tests]

main :: IO ()
main = defaultMain all_tests

-------------------------------------------------------------
