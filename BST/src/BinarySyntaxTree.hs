module BinarySyntaxTree (BST, Key, Value, MaybeValue, lookupBST) where

import Prelude hiding (lookup)

type Key = Int
type Value = String

data BST = InternalNode Key Value BST BST | Leaf

data MaybeBST = JustBST BST | NothingBST
data MaybeKV = JustKV Key Value | NothingKV
data MaybeValue = JustValue Value | NothingValue
data MaybeKey = JustKey Key | NothingKey

lookupBST :: Key -> BST -> MaybeValue
lookupBST soughtKey Leaf = NothingValue
lookupBST soughtKey (InternalNode key item leftChild rightChild) =
  if soughtKey == key
    then JustValue item
    else if soughtKey < key
      then lookupBST soughtKey leftChild
      else lookupBST soughtKey rightChild