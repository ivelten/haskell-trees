module Trees where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node x lt rt) = Node (f x) (fmap f lt) (fmap f rt)

instance Foldable Tree where
  foldr _ acc Leaf = acc
  foldr f acc (Node x lt rt) =
    let acc'  = foldr f acc rt
        acc'' = f x acc'
    in foldr f acc'' lt
  foldl _ acc Leaf = acc
  foldl f acc (Node x lt rt) =
    let acc'  = foldl f acc lt
        acc'' = f acc' x
    in foldl f acc'' rt

depth :: Tree a -> Int
depth Leaf = 0
depth (Node _ lt rt) = 1 + max (depth lt) (depth rt)

isSortedR :: (Ord a, Bounded a) => Tree a -> Bool
isSortedR t = helper t minBound maxBound
  where
    helper Leaf _ _ = True
    helper (Node x lt rt) minVal maxVal =
      let leftSorted  = helper lt minVal x
          rightSorted = helper rt x maxVal
      in x >= minVal && x < maxVal && leftSorted && rightSorted

isSortedL :: (Ord a, Bounded a) => Tree a -> Bool
isSortedL t = helper t minBound maxBound
  where
    helper Leaf _ _ = True
    helper (Node x lt rt) minVal maxVal =
      let rightSorted = helper rt minVal x
          leftSorted  = helper lt x maxVal
      in x >= minVal && x < maxVal && leftSorted && rightSorted

addNewMaxR :: (Integral a) => Tree a -> Tree a
addNewMaxR Leaf = Node 0 Leaf Leaf
addNewMaxR (Node x lt Leaf) = Node x lt (Node (x + 1) Leaf Leaf)
addNewMaxR (Node x lt rt) = Node x lt (addNewMaxR rt)

addNewMaxL :: (Integral a) => Tree a -> Tree a
addNewMaxL Leaf = Node 0 Leaf Leaf
addNewMaxL (Node x Leaf rt) = Node x (Node (x + 1) Leaf Leaf) rt
addNewMaxL (Node x lt rt) = Node x (addNewMaxL lt) rt

toListR :: Tree a -> [a]
toListR = foldr (:) []

toListL :: Tree a -> [a]
toListL = foldl (flip (:)) []

insertR :: (Ord a) => a -> Tree a -> Tree a
insertR x Leaf = Node x Leaf Leaf
insertR x (Node y lt rt) =
  if x > y then Node y lt (insertR x rt)
  else Node y (insertR x lt) rt

insertL :: (Ord a) => a -> Tree a -> Tree a
insertL x Leaf = Node x Leaf Leaf
insertL x (Node y lt rt) =
  if x > y then Node y (insertL x lt) rt
  else Node y lt (insertL x rt)
