import Test.QuickCheck hiding (output)
import Test.Tasty            
import Test.Tasty.QuickCheck hiding (output)
import Test.Tasty.HUnit      

import Prelude hiding (lookup)
import Data.List (nubBy)

import BinarySearchTree (MaybeValue(..))

import Dictionary (Dictionary, empty, lookup, insert, insertMultiple, output, delete)

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (Dictionary k v) where
  arbitrary = do
    key <- arbitrary
    value <- arbitrary
    frequency [(1, return empty), (1, return (insert key value empty))]

-------------------------------------------------------------

-- helper functions

generateUniquePairs :: (Eq k, Arbitrary k, Arbitrary v) => [(k, v)] -> [(k, v)]
generateUniquePairs = nubBy (\x y -> fst x == fst y)

-------------------------------------------------------------

-- property-based tests

prop_empty :: (Ord k, Arbitrary k) => k -> Bool
prop_empty key = 
  lookup key (empty :: Dictionary k v) == NothingValue

prop_multiple_empty :: (Ord k, Arbitrary k) => [k] -> Bool
prop_multiple_empty keys = 
  all (\key -> lookup key (empty :: Dictionary k v) == NothingValue) keys

prop_insert :: (Ord k, Arbitrary k, Arbitrary v, Eq v) => k -> v -> Dictionary k v -> Bool
prop_insert key value dict = 
  lookup key (insert key value dict) == JustValue value

prop_insert_multiple :: (Ord k, Arbitrary k, Arbitrary v, Eq v) => [(k, v)] -> Bool
prop_insert_multiple keyValues = 
  let uniqueKeyValues = generateUniquePairs keyValues
  in all (\(key, value) -> lookup key (insert key value (empty :: Dictionary k v)) == JustValue value) uniqueKeyValues

prop_output_single :: (Ord k, Arbitrary k, Arbitrary v, Eq v) => k -> v -> Dictionary k v -> Bool
prop_output_single key value dict = 
  let newDict = insert key value dict
      outputList = output newDict
  in any (\(k, v) -> k == key && v == value) outputList

prop_output_multiple :: (Ord k, Arbitrary k, Arbitrary v, Eq v) => [(k, v)] -> Dictionary k v -> Bool
prop_output_multiple pairs dict = 
  let uniquePairs = generateUniquePairs pairs
      newDict = insertMultiple uniquePairs dict
      outputList = output newDict
  in all (\(key, value) -> any (\(k, v) -> k == key && v == value) outputList) uniquePairs

prop_delete :: (Ord k, Arbitrary k, Arbitrary v, Eq v) => k -> v -> Dictionary k v -> Bool
prop_delete key value dict = 
  let newDict = insert key value dict
  in lookup key (delete key newDict) == NothingValue

prop_delete_multiple :: (Ord k, Arbitrary k, Arbitrary v, Eq v) => [(k, v)] -> Bool
prop_delete_multiple keyValues = 
  all (\(key, value) -> lookup key (delete key (insert key value empty :: Dictionary k v)) == NothingValue) keyValues

qcheck_tests :: TestTree
qcheck_tests = testGroup "QuickCheck tests"
  [ 
    testProperty "lookup on empty dictionary" prop_empty,
    testProperty "multiple lookups on empty dictionary" prop_multiple_empty,
    testProperty "insert then lookup" prop_insert,
    testProperty "insert multiple then lookup" prop_insert_multiple,
    testProperty "output single pair" prop_output_single,
    testProperty "output multiple pairs" prop_output_multiple,
    testProperty "insert then delete then lookup" prop_delete,
    testProperty "insert multiple then delete then lookup" prop_delete_multiple
  ]

-------------------------------------------------------------

-- unit tests

type IntStringDict = Dictionary Int String

emptyIntStringDict :: IntStringDict
emptyIntStringDict = empty

singleDict :: Dictionary Int String
singleDict = insert 1 "foo" emptyIntStringDict

duoDict :: Dictionary Int String
duoDict = insertMultiple [(1, "foo"), (2, "bar")] emptyIntStringDict

test_output_empty :: Assertion
test_output_empty = 
  assertEqual "" [] (output emptyIntStringDict)

test_lookup_wrong :: Assertion
test_lookup_wrong = 
  assertEqual "" NothingValue (lookup 2 singleDict)

test_insert_multiple_same_key :: Assertion
test_insert_multiple_same_key =
  assertEqual "" (JustValue "foo") (lookup 1 duoDict)

test_insert_multiple_unique_key :: Assertion
test_insert_multiple_unique_key = 
  assertEqual "" (JustValue "bar") (lookup 2 duoDict)

test_delete_empty :: Assertion
test_delete_empty = 
  assertEqual "" NothingValue (lookup 1 (delete 1 emptyIntStringDict))

test_delete_wrong :: Assertion
test_delete_wrong = 
  assertEqual "" (JustValue "foo") (lookup 1 (delete 2 singleDict))

test_delete_root_one_child :: Assertion
test_delete_root_one_child = 
  insert 2 "bar" emptyIntStringDict @?= 
    delete 1 duoDict

test_delete_root_two_children :: Assertion
test_delete_root_two_children = 
  insertMultiple [(1, "apple"), (3, "cherry")] emptyIntStringDict @?= 
    delete 2 (insertMultiple [(2, "banana"), (1, "apple"), (3, "cherry")] emptyIntStringDict)

hunit_tests :: TestTree
hunit_tests = testGroup "HUnit tests"
  [ 
    testCase "output of empty dictionary" test_output_empty,
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