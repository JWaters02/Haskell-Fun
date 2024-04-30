module RecursiveDataTypes where

import Prelude hiding (lookup)

data List t = Node t (List t) | Null

listA :: List Int
listA = Node 1 (Node 2 (Node 3 (Node 4 (Node 5 Null))))

search :: Int -> List Int -> Bool
search soughtItem Null = False
search soughtItem (Node item next) =
   if soughtItem == item
     then True
     else search soughtItem next


pushFront :: t -> List t -> List t
pushFront newItem list = Node newItem list

pushBack :: t -> List t -> List t
pushBack newItem Null = Node newItem Null
pushBack newItem (Node item next) =
  let newNext = pushBack newItem next
   in Node item newNext

listB :: List Int
listB = pushFront 8 listA

listC :: List Int
listC = pushBack 6 listA

--------------------------------------------------------

data BST = InternalNode Int String BST BST | Leaf

-- data Maybe t = Just t | Nothing

lookup :: Int -> BST -> Maybe String
lookup soughtKey Leaf = Nothing
lookup soughtKey (InternalNode key item leftChild rightChild) =
  if soughtKey == key
    then Just item
    else undefined -- ...
