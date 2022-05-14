{- These problems are generally expected to require
   pattern-matching and recursion.  See Inclass.hs
   for week3-datatypes-and-recursion (under Files
   on ICON) for related examples.
-}
module Exercises(module Exercises,module BinTree) where
import BinTree

{- return the data stored in the BinTree, using a
   postfix traversal (so the value stored at each
   node appears after values in its subtrees).  We
   did similar examples in class week3; see
   Files/week3-datatypes-and-recursion/Inclass.hs
   on ICON. -}
toListPost :: BinTree a -> [a]
toListPost Leaf = []
toListPost (Node x l r) = toListPost l ++ toListPost r ++ [x]

{- the output tree should be just like the input
   tree, except that the left and right subtrees of
   every node have been switched. -}
mirror :: BinTree a -> BinTree a
mirror Leaf = Leaf
mirror (Node x l r) = Node x (mirror r) (mirror l)

{- implement a function similar to map for lists, but working on BinTrees.
   In more detail: btMap takes in a function from a to b, and should apply
   that function to all the values stored in the input BinTree, to obtain
   the output BinTree. -}
btMap :: (a -> b) -> BinTree a -> BinTree b
btMap f Leaf = Leaf
btMap f (Node x l r) = Node (f x) (btMap f l) (btMap f r)

{- swap the first and second elements of the list, then
   repeat.  When you get to a list of with just one value
   or the empty list, return that list. -}
swap2 :: [a] -> [a]
swap2 [] = []
swap2 [a] = [a]
swap2 (x:xs) = head xs : x : swap2 (tail xs)

{- combine the two lists by taking one element from the first,
   then an element from the second, and so forth.  If one list
   ends before the other, return the other. -}
knit :: [a] -> [a] -> [a]
knit [] [] = []
knit xs []  = xs
knit [] ys = ys
knit (x:xs) (y:ys) = x : y : knit xs ys

{- return the top part of the tree, cutting off
   at the given Int, which you may assume is greater than or equal to zero -}
btTake :: Int -> BinTree a -> BinTree a
btTake 0 _  = Leaf
btTake i (Node x l r) = Node x (btTake (i-1) l) (btTake (i-1) r)

{- return all the subtrees you find at the given depth,
   which you may assume is greater than or equal to zero.

   At depth 0, you should return a list containing the input tree.

   For depth greater than 0, if the input tree is a Leaf, return
   the empty list.
-}
btDrop :: Int -> BinTree a -> [BinTree a]
btDrop 0 bt = [bt]
btDrop _ Leaf = []
btDrop i (Node x l r) = btDrop (i-1) l ++ btDrop (i-1) r

{- this is like zipWith on lists.
   The given function should be applied to corresponding values
   at Nodes.  If one tree has a Node and the other has a Leaf,
   just return Leaf. -}
btZipWith :: (a -> b -> c) -> BinTree a -> BinTree b -> BinTree c
btZipWith f _ Leaf = Leaf
btZipWith f Leaf _ = Leaf
btZipWith f (Node x1 l1 r1) (Node x2 l2 r2) = Node (f x1 x2) (btZipWith f l1 l2) (btZipWith f r1 r2)

{- We can represent paths into a tree as a list of booleans.  Each
   Bool indicates whether we should recurse into the left subtree (True) or
   the right one (False).  Given a path and a tree, return the subtree found
   by following the path.  If the tree ends in a leaf before the path ends,
   then return Nothing. -}
btSubtree :: [Bool] -> BinTree a -> Maybe (BinTree a)
btSubtree _ Leaf = Nothing
btSubtree [] bt = Just bt
btSubtree (bool:bools) (Node x l r) = if bool == True then (btSubtree bools l) else (btSubtree bools r)

{- btSubst p t1 t2

   Given a path p (as described in the previous problem),
   replace the subtree of t1 that you find following p,
   with t2.  So you are replacing one tree with another,
   inside t1.  If the path is not a valid one into the
   tree (that is, if btSubtree p t1 would return Nothing),
   then just return t1. -}
btSubst :: [Bool] -> BinTree a -> BinTree a -> BinTree a
btSubst [] t1 t2 = t2
btSubst _ t1 Leaf = t1
btSubst p (Node x1 l1 r1) (Node x2 l2 r2) =
   if head p == True
   then (Node x1 ((btSubst (tail p) l1 (Node x2 l2 r2))) r1)
   else (Node x1 l1 (btSubst (tail p) r1 (Node x2 l2 r2)))


