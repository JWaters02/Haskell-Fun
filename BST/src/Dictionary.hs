module Dictionary (Dictionary, empty, lookup, insert, insertMultiple, output, delete) where

import Prelude hiding (lookup)

import BinarySearchTree (BST(..), MaybeValue(..), MaybeKV(..), Key, Value)
import qualified BinarySearchTree as BST (lookup, insert, output, delete)

data Dictionary = Dictionary BST
    deriving (Show, Eq)

empty :: Dictionary
empty = Dictionary Leaf

lookup :: Key -> Dictionary -> MaybeValue
lookup key (Dictionary bst) = BST.lookup key bst

insert :: Key -> Value -> Dictionary -> Dictionary
insert key value (Dictionary bst) = Dictionary (BST.insert key value bst)

insertMultiple :: [(Key, Value)] -> Dictionary -> Dictionary
insertMultiple pairs (Dictionary bst) = Dictionary (foldr (uncurry BST.insert) bst pairs)

output :: Dictionary -> [MaybeKV]
output (Dictionary bst) = BST.output bst

delete :: Key -> Dictionary -> Dictionary
delete key (Dictionary bst) = Dictionary (BST.delete key bst)