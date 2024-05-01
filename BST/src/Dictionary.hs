module Dictionary (Dictionary, empty, lookup, insert, delete) where

import Prelude hiding (lookup)

import BinarySearchTree (BST(..), MaybeValue(..), Key, Value)
import qualified BinarySearchTree as BST (delete, lookup, insert)

data Dictionary = Dictionary BST
    deriving (Show, Eq)

empty :: Dictionary
empty = Dictionary Leaf

lookup :: Key -> Dictionary -> MaybeValue
lookup key (Dictionary bst) = BST.lookup key bst

insert :: Key -> Value -> Dictionary -> Dictionary
insert key value (Dictionary bst) = Dictionary (BST.insert key value bst)

delete :: Key -> Dictionary -> Dictionary
delete key (Dictionary bst) = Dictionary (BST.delete key bst)