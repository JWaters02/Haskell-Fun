import Test.QuickCheck       
import Test.Tasty            
import Test.Tasty.QuickCheck 
import Test.Tasty.HUnit      

import Prelude hiding (lookup)

import BinarySearchTree (MaybeValue(..), Key, Value)

import Dictionary (Dictionary, empty, lookup, insert, insertMultiple, delete)

instance Arbitrary Dictionary where
  arbitrary = do
    key <- arbitrary
    value <- arbitrary
    frequency [(1, return empty), (1, return (insert key value empty))]

-------------------------------------------------------------

-- property-based tests

prop_empty :: Key -> Bool
prop_empty key = 
  lookup key empty == NothingValue

prop_multiple_empty :: [Key] -> Bool
prop_multiple_empty keys = 
  all (\key -> lookup key empty == NothingValue) keys

prop_insert :: Key -> Value -> Dictionary -> Bool
prop_insert key value dict = 
  lookup key (insert key value dict) == JustValue value

prop_insert_multiple :: [(Key, Value)] -> Bool
prop_insert_multiple keyValues = 
  all (\(key, value) -> lookup key (insert key value empty) == JustValue value) keyValues

prop_delete :: Key -> Value -> Dictionary -> Bool
prop_delete key value dict = 
  lookup key (delete key (insert key value dict)) == NothingValue

prop_delete_multiple :: [(Key, Value)] -> Bool
prop_delete_multiple keyValues = 
  all (\(key, value) -> lookup key (delete key (insert key value empty)) == NothingValue) keyValues

qcheck_tests :: TestTree
qcheck_tests = testGroup "QuickCheck tests"
  [ 
    testProperty "lookup on empty dictionary" prop_empty,
    testProperty "multiple lookups on empty dictionary" prop_multiple_empty,
    testProperty "insert then lookup" prop_insert,
    testProperty "insert multiple then lookup" prop_insert_multiple,
    testProperty "insert then delete then lookup" prop_delete,
    testProperty "insert multiple then delete then lookup" prop_delete_multiple
  ]

-------------------------------------------------------------

-- unit tests

basicDict :: Dictionary
basicDict = insert 1 "foo" empty

duoDict :: Dictionary
duoDict = insertMultiple [(1, "foo"), (2, "bar")] empty

test_lookup_wrong :: Assertion
test_lookup_wrong = 
  assertEqual "" NothingValue (lookup 2 basicDict)

test_insert_multiple_same_key :: Assertion
test_insert_multiple_same_key =
  assertEqual "" (JustValue "foo") (lookup 1 duoDict)

test_insert_multiple_unique_key :: Assertion
test_insert_multiple_unique_key = 
  assertEqual "" (JustValue "bar") (lookup 2 duoDict)

test_delete_empty :: Assertion
test_delete_empty = 
  assertEqual "" NothingValue (lookup 1 (delete 1 empty))

test_delete_wrong :: Assertion
test_delete_wrong = 
  assertEqual "" (JustValue "foo") (lookup 1 (delete 2 basicDict))

test_delete_root_one_child :: Assertion
test_delete_root_one_child = 
  insert 2 "bar" empty @?= 
    delete 1 duoDict

test_delete_root_two_children :: Assertion
test_delete_root_two_children = 
  insertMultiple [(1, "apple"), (3, "cherry")] empty @?= 
    delete 2 (insertMultiple [(2, "banana"), (1, "apple"), (3, "cherry")] empty)

hunit_tests :: TestTree
hunit_tests = testGroup "HUnit tests"
  [ 
    testCase "insert then lookup wrong key" test_lookup_wrong,
    testCase "insert multiple same key then lookup" test_insert_multiple_same_key,
    testCase "insert multiple unique key then lookup" test_insert_multiple_unique_key,
    testCase "delete empty dictionary" test_delete_empty,
    testCase "insert then delete wrong key then lookup" test_delete_wrong,
    testCase "delete root with one child" test_delete_root_one_child,
    testCase "delete root with two children" test_delete_root_two_children
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