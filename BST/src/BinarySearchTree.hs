module BinarySearchTree (
  BST(..), 
  Key, Value,
  MaybeValue(..), 
  lookup,
  insert,
  delete
) where

import Prelude hiding (lookup)

type Key = Int
type Value = String

data BST = InternalNode Key Value BST BST | Leaf
  deriving (Show, Eq)

data MaybeValue = JustValue Value | NothingValue
  deriving (Show, Eq)

lookup :: Key -> BST -> MaybeValue
lookup _ Leaf = NothingValue
lookup soughtKey (InternalNode key item leftChild rightChild) =
  if soughtKey == key
    then JustValue item
    else if soughtKey < key
      then lookup soughtKey leftChild
      else lookup soughtKey rightChild

insert :: Key -> Value -> BST -> BST
insert key value Leaf = InternalNode key value Leaf Leaf
insert key value (InternalNode nodeKey nodeValue leftChild rightChild) =
  if key == nodeKey
    then InternalNode key value leftChild rightChild
    else if key < nodeKey
      then InternalNode nodeKey nodeValue (insert key value leftChild) rightChild
      else InternalNode nodeKey nodeValue leftChild (insert key value rightChild)

delete :: Key -> BST -> BST
delete _ _ = error "delete not implemented yet"