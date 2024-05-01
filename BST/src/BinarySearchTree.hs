module BinarySearchTree (
  BST(..), 
  Key, Value,
  MaybeValue(..), 
  MaybeKV(..),
  lookup,
  insert,
  output,
  delete
) where

import Prelude hiding (lookup)

type Key = Int
type Value = String

data BST = InternalNode Key Value BST BST | Leaf
  deriving (Show, Ord, Eq)

data MaybeValue = JustValue Value | NothingValue
  deriving (Show, Eq)

data MaybeKV = JustKV Key Value | NothingKV
  deriving (Show, Eq)

lookup :: Key -> BST -> MaybeValue
lookup _ Leaf = NothingValue
lookup key (InternalNode nodeKey nodeValue leftChild rightChild)
  | key == nodeKey = JustValue nodeValue
  | key < nodeKey = lookup key leftChild
  | otherwise = lookup key rightChild

insert :: Key -> Value -> BST -> BST
insert key value Leaf = InternalNode key value Leaf Leaf
insert key value (InternalNode nodeKey nodeValue leftChild rightChild)
  | key == nodeKey = InternalNode key value leftChild rightChild
  | key < nodeKey = InternalNode nodeKey nodeValue (insert key value leftChild) rightChild
  | otherwise = InternalNode nodeKey nodeValue leftChild (insert key value rightChild)

output :: BST -> [MaybeKV]
output _ = error "Not implemented"

isLeaf :: BST -> Bool
isLeaf Leaf = True
isLeaf _ = False

maxNode :: BST -> (Key, Value)
maxNode (InternalNode key value Leaf _) = (key, value)
maxNode (InternalNode _ _ _ rightChild) = maxNode rightChild

delete :: Key -> BST -> BST
delete _ Leaf = Leaf
delete key (InternalNode nodeKey nodeValue leftChild rightChild)
  | key < nodeKey = InternalNode nodeKey nodeValue (delete key leftChild) rightChild
  | key > nodeKey = InternalNode nodeKey nodeValue leftChild (delete key rightChild)
  | isLeaf leftChild = rightChild
  | isLeaf rightChild = leftChild
  | otherwise = 
    let (prevKey, prevValue) = maxNode leftChild
    in InternalNode prevKey prevValue leftChild (delete prevKey rightChild)