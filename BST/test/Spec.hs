import Test.QuickCheck       
import Test.Tasty            
import Test.Tasty.QuickCheck 
import Test.Tasty.HUnit      

import Prelude hiding (lookup)

import BinarySearchTree (MaybeValue(..), Key, Value)

import Dictionary (Dictionary, empty, lookup, insert, delete)

instance Arbitrary Dictionary where
  arbitrary = do
    key <- arbitrary
    value <- arbitrary
    frequency [(1, return empty), (1, return (insert key value empty))]

-------------------------------------------------------------

-- property-based tests

prop_empty :: Key -> Bool
prop_empty key = lookup key empty == NothingValue

prop_multiple_emptys :: [Key] -> Bool
prop_multiple_emptys keys = all (\key -> lookup key empty == NothingValue) keys

prop_insert :: Key -> Value -> Dictionary -> Bool
prop_insert key value dict = lookup key (insert key value dict) == JustValue value

prop_insert_multiple :: [(Key, Value)] -> Bool
prop_insert_multiple keyValues = all (\(key, value) -> lookup key (insert key value empty) == JustValue value) keyValues

prop_delete :: Key -> Value -> Dictionary -> Bool
prop_delete key value dict = lookup key (delete key (insert key value dict)) == NothingValue

prop_delete_multiple :: [(Key, Value)] -> Bool
prop_delete_multiple keyValues = all (\(key, value) -> lookup key (delete key (insert key value empty)) == NothingValue) keyValues

qcheck_tests :: TestTree
qcheck_tests = testGroup "QuickCheck tests"
  [ 
    testProperty "lookup on empty dictionary" prop_empty,
    testProperty "multiple lookups on empty dictionary" prop_multiple_emptys,
    testProperty "insert then lookup on dictionary" prop_insert,
    testProperty "insert multiple then lookup on dictionary" prop_insert_multiple,
    testProperty "insert then delete then lookup on dictionary" prop_delete
    -- testProperty "insert multiple then delete then lookup on dictionary" prop_delete_multiple
  ]

-------------------------------------------------------------

-- unit tests

test_empty :: Assertion
test_empty = assertEqual "" NothingValue (lookup 1 empty)

test_insert :: Assertion
test_insert = assertEqual "" (JustValue "foo") (lookup 1 (insert 1 "foo" empty))

test_insert_multiple_same_key :: Assertion
test_insert_multiple_same_key = assertEqual "" (JustValue "foo") (lookup 1 (insert 1 "foo" (insert 1 "bar" empty)))

test_insert_multiple_unique_key :: Assertion
test_insert_multiple_unique_key = assertEqual "" (JustValue "bar") (lookup 2 (insert 1 "foo" (insert 2 "bar" empty)))

test_delete :: Assertion
test_delete = assertEqual "" NothingValue (lookup 1 (delete 1 (insert 1 "foo" empty)))

test_delete_wrong :: Assertion
test_delete_wrong = assertEqual "" (JustValue "foo") (lookup 1 (delete 2 (insert 1 "foo" empty)))

test_delete_multiple :: Assertion
test_delete_multiple = assertEqual "" (JustValue "bar") (lookup 2 (delete 1 (insert 1 "foo" (insert 2 "bar" empty))))

hunit_tests :: TestTree
hunit_tests = testGroup "HUnit tests"
  [ 
    testCase "lookup on empty dictionary" test_empty,
    testCase "insert then lookup on dictionary" test_insert,
    testCase "insert multiple same key then lookup on dictionary" test_insert_multiple_same_key,
    testCase "insert multiple unique key then lookup on dictionary" test_insert_multiple_unique_key,
    -- testCase "insert then delete then lookup on dictionary" test_delete,
    testCase "insert then delete wrong key then lookup on dictionary" test_delete_wrong
    -- testCase "insert multiple then delete then lookup on dictionary" test_delete_multiple
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