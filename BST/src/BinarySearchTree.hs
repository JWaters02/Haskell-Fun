module BinarySearchTree (
  BST(..), 
  Key, Value,
  MaybeValue(..), 
  lookupBST,
  insertBST
) where

type Key = Int
type Value = String

data BST = InternalNode Key Value BST BST | Leaf
  deriving (Show, Eq)

data MaybeValue = JustValue Value | NothingValue
  deriving (Show, Eq)

lookupBST :: Key -> BST -> MaybeValue
lookupBST soughtKey Leaf = NothingValue
lookupBST soughtKey (InternalNode key item leftChild rightChild) =
  if soughtKey == key
    then JustValue item
    else if soughtKey < key
      then lookupBST soughtKey leftChild
      else lookupBST soughtKey rightChild

insertBST :: Key -> Value -> BST -> BST
insertBST key value Leaf = InternalNode key value Leaf Leaf
insertBST key value (InternalNode nodeKey nodeValue leftChild rightChild) =
  if key == nodeKey
    then InternalNode key value leftChild rightChild
    else if key < nodeKey
      then InternalNode nodeKey nodeValue (insertBST key value leftChild) rightChild
      else InternalNode nodeKey nodeValue leftChild (insertBST key value rightChild)