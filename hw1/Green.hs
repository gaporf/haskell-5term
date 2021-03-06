{-# LANGUAGE InstanceSigs #-}

module Green
  (
    Tree
  , addElement
  , countElements
  , deleteElement
  , findElement
  , foldr
  , fromList
  , isEmpty
  ) where

import Data.List.NonEmpty as NonEmpty hiding (fromList)
import Prelude hiding (foldr)
import qualified Prelude (foldr)

-- | Tree representation
data Tree a
  = Branch (NonEmpty a) (Tree a) (Tree a)
  | Leaf
  deriving (Show)

instance Foldable Tree where
  foldMap
    :: Monoid m
    => (a -> m)
    -> Tree a
    -> m
  foldMap _ Leaf            = mempty
  foldMap f (Branch xs l r) = (foldMap f r) `mappend` (foldMap f xs) `mappend` (foldMap f l)

-- | Return a new Tree with added element. It is guaranteed if the initial
-- Tree contains an added element, new Tree will be equal to initial one.
addElement
  :: Ord a
  => Tree a  -- ^ Initial Tree
  -> a       -- ^ Element to add
  -> Tree a  -- ^ New Tree with element
addElement Leaf x = Branch (x :| []) Leaf Leaf
addElement (Branch xs@(x :| _) l r) e
  | x < e     = Branch xs (addElement l e) r
  | x == e    = Branch (e <| xs) l r
  | otherwise = Branch xs l (addElement r e)

-- | Return the number of elements in Tree
countElements
  :: Tree a  -- ^ Input Tree
  -> Int     -- ^ The number of elements in input tree
countElements Leaf = 0
countElements (Branch xs l r) =
  (countElements l) +
  (countElements r) +
  (NonEmpty.length xs
  )
-- | Return a new Tree without deleted element. It is guaranteed if the
-- initial Tree does not contain deleted element, new Tree will be
-- equal to initial one.
deleteElement
  :: Ord a
  => Tree a  -- ^ Initial Tree
  -> a       -- ^ Element to delete
  -> Tree a  -- ^ Tree without element
deleteElement Leaf _ = Leaf
deleteElement (Branch xs@(x :| _) l r) e
  | x < e     = Branch xs (deleteElement l e) r
  | x == e    = deleteRootElement $ Branch xs l r
  | otherwise = Branch xs l (deleteElement r e)

-- | Determine if some element belong to the tree
findElement
  :: Ord a
  => Tree a  -- ^ Input Tree
  -> a       -- ^ The element to find in input tree
  -> Bool    -- ^ True if input Tree contains element otherwise False
findElement Leaf _ = False
findElement (Branch (x :| _) l r) e
  | x < e     = findElement l e
  | x == e    = True
  | otherwise = findElement r e

-- | Return the result of function and initial element applied to Tree
foldr
  :: (a -> b -> b)  -- ^ Function, applied to Tree
  -> b              -- ^ Initial element
  -> Tree a         -- ^ Tree, which is applied function
  -> b              -- ^ The result of function
foldr _ initial Leaf = initial
foldr f initial (Branch xs l r) =
  let first = foldr f initial r
  in let second = Prelude.foldr f first xs
     in foldr f second l

-- | Return a new Tree from input list
fromList
  :: Ord a
  => [a]     -- ^ Input list
  -> Tree a  -- ^ New Tree created from input list
fromList []       = Leaf
fromList (x : xs) = addElement (Green.fromList xs) x

-- | Determine if the tree is empty
isEmpty
  :: Tree a  -- ^ Input Tree
  -> Bool    -- ^ True if Tree is empty otherwise False
isEmpty Leaf = True
isEmpty _    = False

deleteRootElement
  :: Ord a
  => Tree a
  -> Tree a
deleteRootElement Leaf                                = undefined
deleteRootElement (Branch (_ :| (x : xs)) left right) = Branch (x :| xs) left right
deleteRootElement (Branch _ left right)               = addFrom left right

addFrom
  :: Ord a
  => Tree a
  -> Tree a
  -> Tree a
addFrom Leaf right = right
addFrom (Branch (x :| (y : ys)) left right) to = addFrom (Branch (y :| ys) left right) (addElement to x)
addFrom (Branch (x :| []) left right) to =
  let firstTree = addFrom left (addElement to x)
  in addFrom right firstTree
