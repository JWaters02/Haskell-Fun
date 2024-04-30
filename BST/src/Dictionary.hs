module Dictionary (Dictionary, empty, lookup) where

import Prelude hiding (lookup)

import BinarySyntaxTree (
    BST(..), 
    MaybeValue(..), 
    Key, Value,
    lookupBST)

data Dictionary = Dictionary BST
    deriving (Show, Eq)

empty :: Dictionary
empty = Dictionary Leaf

lookup :: Key -> Dictionary -> MaybeValue
lookup key (Dictionary bst) = lookupBST key bst