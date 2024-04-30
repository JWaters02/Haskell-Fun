module List (List,search,pushFront,pushBack,empty) where

data List t = Node t (List t) | Null

empty :: List t
empty = Null

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
