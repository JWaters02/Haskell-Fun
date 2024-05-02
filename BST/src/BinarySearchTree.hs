module BinarySearchTree (
  BST(..),
  MaybeValue(..), 
  lookup,
  insert,
  output,
  delete, 
  deleteOnPredicate
) where

import Prelude hiding (lookup)

data BST k v = InternalNode k v (BST k v) (BST k v) | Leaf
  deriving (Show, Ord, Eq)

data MaybeValue v = JustValue v | NothingValue
  deriving (Show, Eq)

lookup :: (Ord k) => k -> BST k v -> MaybeValue v
lookup _ Leaf = NothingValue
lookup key (InternalNode nodeKey nodeValue leftChild rightChild)
  | key == nodeKey = JustValue nodeValue
  | key < nodeKey = lookup key leftChild
  | otherwise = lookup key rightChild

insert :: (Ord k) => k -> v -> BST k v -> BST k v
insert key value Leaf = InternalNode key value Leaf Leaf
insert key value (InternalNode nodeKey nodeValue leftChild rightChild)
  | key == nodeKey = InternalNode key value leftChild rightChild
  | key < nodeKey = InternalNode nodeKey nodeValue (insert key value leftChild) rightChild
  | otherwise = InternalNode nodeKey nodeValue leftChild (insert key value rightChild)

output :: BST k v -> [(k, v)]
output Leaf = []
output (InternalNode key value leftChild rightChild) = 
  output leftChild ++ [(key, value)] ++ output rightChild

isLeaf :: BST k v -> Bool
isLeaf Leaf = True
isLeaf _ = False

maxNode :: BST k v -> (k, v)
maxNode (InternalNode key value Leaf _) = (key, value)
maxNode (InternalNode _ _ _ rightChild) = maxNode rightChild

delete :: (Ord k) => k -> BST k v -> BST k v
delete _ Leaf = Leaf
delete key (InternalNode nodeKey nodeValue leftChild rightChild)
  | key < nodeKey = InternalNode nodeKey nodeValue (delete key leftChild) rightChild
  | key > nodeKey = InternalNode nodeKey nodeValue leftChild (delete key rightChild)
  | isLeaf leftChild = rightChild
  | isLeaf rightChild = leftChild
  | otherwise = 
    let (prevKey, prevValue) = maxNode leftChild
    in InternalNode prevKey prevValue leftChild (delete prevKey rightChild)

deleteOnPredicate :: (Ord k) => (k -> v -> Bool) -> BST k v -> BST k v
deleteOnPredicate _ Leaf = Leaf
deleteOnPredicate pred (InternalNode key value leftChild rightChild)
  | pred key value = delete key (deleteOnPredicate pred leftChild)
  | otherwise = InternalNode key value (deleteOnPredicate pred leftChild) (deleteOnPredicate pred rightChild)