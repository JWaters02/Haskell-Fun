{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck hiding (output)
import Test.Tasty            
import Test.Tasty.QuickCheck hiding (output)
import Test.Tasty.HUnit      

import Prelude hiding (lookup)
import Data.List (nubBy)

import BinarySearchTree (MaybeValue(..))

import Dictionary (Dictionary, empty, lookup, insert, insertMultiple, output, delete, deleteOnPredicate)

type Key = Int
type Value = String

instance Arbitrary (Dictionary Key Value) where
  arbitrary = do
    key <- arbitrary
    value <- arbitrary
    frequency [(1, return empty), (9, return (insert key value empty))]

type IntStringDict = Dictionary Int String
emptyIntStringDict :: IntStringDict
emptyIntStringDict = empty

type CharIntDict = Dictionary Char Int
emptyCharIntDict :: CharIntDict
emptyCharIntDict = empty

-------------------------------------------------------------

-- helper functions

generateUniquePairs :: (Eq k) => [(k, v)] -> [(k, v)]
generateUniquePairs = nubBy (\x y -> fst x == fst y)

-------------------------------------------------------------

-- property-based tests

prop_empty :: Key -> Bool
prop_empty key =
  lookup key emptyIntStringDict == NothingValue

prop_multiple_empty :: [Key] -> Bool
prop_multiple_empty keys =
  all (\key -> lookup key emptyIntStringDict == NothingValue) keys

prop_insert :: Key -> String -> Dictionary Key Value -> Bool
prop_insert key value dict =
  lookup key (insert key value dict) == JustValue value

prop_insert_multiple :: [(Key, Value)] -> Bool
prop_insert_multiple keyValues = 
  let uniqueKeyValues = generateUniquePairs keyValues
  in all (\(key, value) -> lookup key (insert key value empty) == JustValue value) uniqueKeyValues

prop_output_single :: Key -> Value -> Dictionary Key Value -> Bool
prop_output_single key value dict = 
  let newDict = insert key value dict
      outputList = output newDict
  in any (\(k, v) -> k == key && v == value) outputList

prop_output_multiple :: [(Key, Value)] -> Dictionary Key Value -> Bool
prop_output_multiple pairs dict = 
  let uniquePairs = generateUniquePairs pairs
      newDict = insertMultiple uniquePairs dict
      outputList = output newDict
  in all (\(key, value) -> any (\(k, v) -> k == key && v == value) outputList) uniquePairs

prop_delete :: Key -> Value -> Dictionary Key Value -> Bool
prop_delete key value dict = 
  let newDict = insert key value dict
  in lookup key (delete key newDict) == NothingValue

prop_delete_multiple :: [(Key, Value)] -> Bool
prop_delete_multiple keyValues = 
  all (\(key, value) -> lookup key (delete key (insert key value empty)) == NothingValue) keyValues

prop_delete_on_predicate :: [(Key, Value)] -> Bool
prop_delete_on_predicate keyValues = 
  let uniqueKeyValues = generateUniquePairs keyValues
      dict = insertMultiple uniqueKeyValues empty
      newDict = deleteOnPredicate (\_ _ -> True) dict
  in all (\(key, _) -> lookup key newDict == NothingValue) uniqueKeyValues

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
    testProperty "insert multiple then delete then lookup" prop_delete_multiple,
    testProperty "delete on predicate removes all" prop_delete_on_predicate
  ]

-------------------------------------------------------------

-- unit tests

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

test_insert_multiple_unique_key_char :: Assertion
test_insert_multiple_unique_key_char = 
  assertEqual "" (JustValue 2) (lookup 'b' (insertMultiple [('a', 1), ('b', 2)] emptyCharIntDict))

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

test_delete_on_predicate_remove_odd :: Assertion
test_delete_on_predicate_remove_odd = 
  insert 2 "banana" emptyIntStringDict @?= 
    deleteOnPredicate (\k _ -> odd k) (insertMultiple [(1, "apple"), (2, "banana"), (3, "cherry")] emptyIntStringDict)

test_delete_on_predicate_remove_none :: Assertion
test_delete_on_predicate_remove_none = 
  insertMultiple [(1, "apple"), (2, "banana"), (3, "cherry")] emptyIntStringDict @?= 
    deleteOnPredicate (\_ _ -> False) (insertMultiple [(1, "apple"), (2, "banana"), (3, "cherry")] emptyIntStringDict)

hunit_tests :: TestTree
hunit_tests = testGroup "HUnit tests"
  [ 
    testCase "output of empty dictionary" test_output_empty,
    testCase "insert then lookup wrong key" test_lookup_wrong,
    testCase "insert multiple same key then lookup" test_insert_multiple_same_key,
    testCase "insert multiple unique key then lookup" test_insert_multiple_unique_key,
    testCase "insert multiple unique key char then lookup" test_insert_multiple_unique_key_char,
    testCase "delete empty dictionary" test_delete_empty,
    testCase "insert then delete wrong key then lookup" test_delete_wrong,
    testCase "delete root with one child" test_delete_root_one_child,
    testCase "delete root with two children" test_delete_root_two_children,
    testCase "delete on predicate removes odd keys" test_delete_on_predicate_remove_odd,
    testCase "delete on predicate removes none" test_delete_on_predicate_remove_none
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