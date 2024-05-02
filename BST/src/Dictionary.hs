module Dictionary (Dictionary, empty, lookup, insert, insertMultiple, output, delete, deleteOnPredicate) where

import Prelude hiding (lookup, pred)

import BinarySearchTree (BST(..), MaybeValue(..))
import qualified BinarySearchTree as BST (lookup, insert, output, delete, deleteOnPredicate)

data Dictionary k v = Dictionary (BST k v)
    deriving (Show, Ord, Eq)

empty :: Dictionary k v
empty = Dictionary Leaf

lookup :: (Ord k) => k -> Dictionary k v -> MaybeValue v
lookup key (Dictionary bst) = BST.lookup key bst

insert :: (Ord k) => k -> v -> Dictionary k v -> Dictionary k v
insert key value (Dictionary bst) = Dictionary (BST.insert key value bst)

insertMultiple :: (Ord k) => [(k, v)] -> Dictionary k v -> Dictionary k v
insertMultiple pairs (Dictionary bst) = Dictionary (foldr (uncurry BST.insert) bst pairs)

output :: Dictionary k v -> [(k, v)]
output (Dictionary bst) = BST.output bst

delete :: (Ord k) => k -> Dictionary k v -> Dictionary k v
delete key (Dictionary bst) = Dictionary (BST.delete key bst)

deleteOnPredicate :: (Ord k) => (k -> v -> Bool) -> Dictionary k v -> Dictionary k v
deleteOnPredicate pred (Dictionary bst) = Dictionary (BST.deleteOnPredicate pred bst)