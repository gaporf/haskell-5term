{-# LANGUAGE InstanceSigs #-}

module Tree
  (
    Tree (Branch, Leaf)
  ) where

-- | Implementaion of the binary tree
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Eq, Show)

instance Functor Tree where
  fmap
    :: (a -> b)
    -> Tree a
    -> Tree b
  fmap f (Leaf x)            = Leaf $ f x
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Applicative Tree where
  pure
    :: a
    -> Tree a
  pure = Leaf
  (<*>)
    :: Tree (a -> b)
    -> Tree a
    -> Tree b
  (Leaf f) <*> tree = fmap f tree
  (Branch left right) <*> tree = Branch (left <*> tree) (right <*> tree)

instance Foldable Tree where
  foldr
    :: (a -> b -> b)
    -> b
    -> Tree a
    -> b
  foldr f ini (Leaf x)            = f x ini
  foldr f ini (Branch left right) = foldr f (foldr f ini right) left

instance Traversable Tree where
  traverse
    :: Applicative f => (a -> f b)
    -> Tree a
    -> f (Tree b)
  traverse f (Leaf x)            = Leaf <$> f x
  traverse f (Branch left right) = Branch <$> traverse f left <*> traverse f right
