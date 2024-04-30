module QSort where

import Test.QuickCheck


qsort :: [Int] -> [Int]
qsort [] = []
qsort [element] = [element]
qsort (pivot:rest) =
  let
      smaller = filter (<pivot) rest
      greater = filter (>=pivot) rest
   in
      qsort smaller ++ [pivot] ++ qsort greater


prop_qsort_idempotent :: [Int] -> Property
prop_qsort_idempotent list =
  property (qsort list == qsort (qsort list))

prop_qsort_preservesLength :: [Int] -> Property
prop_qsort_preservesLength list =
  property (length list == length (qsort list))


main :: IO ()
main = do
  quickCheck prop_qsort_idempotent
  quickCheck prop_qsort_preservesLength
