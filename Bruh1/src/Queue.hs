module Queue (Queue, emptyQ, pushQ, popQ) where

-----------------------------------------------

-- data Queue t = Node t (Queue t) | Null

-- emptyQ :: Queue t
-- emptyQ = Null

-- pushQ :: t -> Queue t -> Queue t
-- pushQ newItem Null = Node newItem Null
-- pushQ newItem (Node item next) =
--   let newNext = pushQ newItem next
--    in Node item newNext

-- popQ :: Queue t -> (t, Queue t)
-- popQ (Node item rest) = (item, rest)

-----------------------------------------------

-- import List

-- data Queue t = Queue (List t)

-- emptyQ :: Queue t
-- emptyQ = Queue (empty)

-- pushQ :: t -> Queue t -> Queue t
-- pushQ item (Queue list) = Queue (pushBack item list)

-- popQ :: Queue t -> (t, Queue t)
-- popQ (Queue (Node item list)) = (item, Queue list)

-----------------------------------------------

data Queue t = Queue [t]

emptyQ :: Queue t
emptyQ = Queue []

pushQ :: t -> Queue t -> Queue t
pushQ item (Queue list) = Queue (list ++ [item])

popQ :: Queue t -> (t, Queue t)
popQ (Queue (item : list)) = (item, Queue list)

-----------------------------------------------
