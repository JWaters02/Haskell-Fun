import Test.QuickCheck       
import Test.Tasty            
import Test.Tasty.QuickCheck 
import Test.Tasty.HUnit      

import Prelude hiding (lookup)

import BinarySearchTree (MaybeValue(..), Key, Value)

import Dictionary (Dictionary, empty, lookup, insert)

instance Arbitrary Dictionary where
  arbitrary = do
    key <- arbitrary
    value <- arbitrary
    frequency [(1, return empty), (3, return (insert key value empty))]

-------------------------------------------------------------

-- property-based tests

prop_empty_lookup :: Key -> Bool
prop_empty_lookup key = lookup key empty == NothingValue

prop_multiple_empty_lookups :: [Key] -> Bool
prop_multiple_empty_lookups keys = all (\key -> lookup key empty == NothingValue) keys

prop_insert_lookup :: Key -> Value -> Dictionary -> Bool
prop_insert_lookup key value dict = lookup key (insert key value dict) == JustValue value

qcheck_tests :: TestTree
qcheck_tests = testGroup "QuickCheck tests"
  [ 
    testProperty "lookup on empty dictionary" prop_empty_lookup,
    testProperty "multiple lookups on empty dictionary" prop_multiple_empty_lookups,
    testProperty "insert then lookup on dictionary" prop_insert_lookup
  ]

-------------------------------------------------------------

-- unit tests

test_empty_lookup :: Assertion
test_empty_lookup = assertEqual "" NothingValue (lookup 1 empty)

test_insert_lookup :: Assertion
test_insert_lookup = assertEqual "" (JustValue "foo") (lookup 1 (insert 1 "foo" empty))

hunit_tests :: TestTree
hunit_tests = testGroup "HUnit tests"
  [ 
    testCase "lookup on empty dictionary" test_empty_lookup,
    testCase "insert then lookup on dictionary" test_insert_lookup
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