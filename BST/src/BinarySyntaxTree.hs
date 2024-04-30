module BinarySyntaxTree (
  BST(..), 
  Key, Value,
  MaybeValue(..), 
  lookupBST
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