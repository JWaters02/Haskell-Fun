module Dictionary (Dictionary, empty, lookup, insert) where

import Prelude hiding (lookup)

import BinarySearchTree (
    BST(..), 
    MaybeValue(..), 
    Key, Value,
    lookupBST, insertBST)

data Dictionary = Dictionary BST
    deriving (Show, Eq)

empty :: Dictionary
empty = Dictionary Leaf

lookup :: Key -> Dictionary -> MaybeValue
lookup key (Dictionary bst) = lookupBST key bst

insert :: Key -> Value -> Dictionary -> Dictionary
insert key value (Dictionary bst) = Dictionary (insertBST key value bst)