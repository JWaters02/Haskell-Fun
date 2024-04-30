module Dictionary (empty, lookup) where

import Prelude hiding (lookup)

import BinarySyntaxTree (BST, Key, Value, MaybeValue, Leaf, lookupBST)

data Dictionary = Dictionary BST

empty :: Dictionary
empty = Dictionary Leaf

lookup :: Key -> Dictionary -> MaybeValue
lookup key (Dictionary bst) = lookupBST key bst