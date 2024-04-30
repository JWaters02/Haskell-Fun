import Test.QuickCheck       
import Test.Tasty            
import Test.Tasty.QuickCheck 
import Test.Tasty.HUnit      

import Dictionary (empty, lookup)

-------------------------------------------------------------

-- property-based tests

prop_empty_lookup :: Key -> Bool
prop_empty_lookup key = lookup key empty == NothingValue



qcheck_tests :: TestTree
qcheck_tests = testGroup "QuickCheck tests"
  [ 
    testProperty "lookup on empty dictionary" prop_empty_lookup
  ]

-------------------------------------------------------------

-- unit tests

test_empty_lookup :: Assertion
test_empty_lookup = assertEqual "" NothingValue (lookup 42 empty)

hunit_tests :: TestTree
hunit_tests = testGroup "HUnit tests"
  [ 
    testCase "lookup on empty dictionary" test_empty_lookup
  ]

-------------------------------------------------------------

all_tests :: TestTree
all_tests = testGroup "All Tests" 
  [
    qcheck_tests,
    hunit_tests
  ]

main :: IO ()
main = defaultMain all_tests