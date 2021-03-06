{-# LANGUAGE InstanceSigs #-}

module Semigroup
  ( NonEmpty ((:|))
  , ThisOrThat (This, That, Both)
  , Name (Name)
  , Endo (Endo, getEndo)
  ) where

-- | Non empty list representation
data NonEmpty a = a :| [a]

instance Show a => Show (NonEmpty a) where
  show
    :: NonEmpty a
    -> String
  show (x :| xs) = show $ x : xs

instance Eq a => Eq (NonEmpty a) where
  (==)
    :: NonEmpty a
    -> NonEmpty a
    -> Bool
  (==) (x :| xs) (y :| ys) = (x == y) && (xs == ys)

instance Semigroup (NonEmpty a) where
  (<>)
    :: NonEmpty a
    -> NonEmpty a
    -> NonEmpty a
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ y : ys)

-- | ThisOrThat representation
data ThisOrThat a b
  = This a
  | That b
  | Both a b

instance (Show a, Show b) => Show (ThisOrThat a b) where
  show
    :: ThisOrThat a b
    -> String
  show (This x)   = "This " ++ (show x)
  show (That y)   = "That " ++ (show y)
  show (Both x y) = "Both " ++ (show x) ++ " and " ++ (show y)

instance (Eq a, Eq b) => Eq (ThisOrThat a b) where
  (==)
    :: ThisOrThat a b
    -> ThisOrThat a b
    -> Bool
  (==) (This x) (This y)     = x == y
  (==) (That x) (That y)     = x == y
  (==) (Both x y) (Both a b) = (x == a) && (y == b)
  (==) _ _                   = False

instance Semigroup (ThisOrThat a b) where
  (<>)
    :: (ThisOrThat a b)
    -> (ThisOrThat a b)
    -> (ThisOrThat a b)
  (<>) _ (Both x y)        = Both x y
  (<>) (This _) (This x)   = This x
  (<>) (That y) (This x)   = Both x y
  (<>) (Both _ y) (This x) = Both x y
  (<>) (This x) (That y)   = Both x y
  (<>) (That _) (That y)   = That y
  (<>) (Both x _) (That y) = Both x y

-- | Name representation
data Name = Name String
  deriving Show

instance Eq Name where
  (==)
    :: Name
    -> Name
    -> Bool
  (==) (Name x) (Name y) = x == y

instance Semigroup Name where
  (<>)
    :: Name
    -> Name
    -> Name
  (<>) (Name "") name    = name
  (<>) name (Name "")    = name
  (<>) (Name a) (Name b) = Name $ a ++ '.' : b

instance Monoid Name where
  mempty :: Name
  mempty = Name ""

-- | Endomorphism representation
newtype Endo a = Endo { getEndo :: a -> a }

instance (Semigroup a) => Semigroup (Endo a) where
  (<>)
    :: Endo a
    -> Endo a
    -> Endo a
  (<>) x y = Endo $ (getEndo x) <> (getEndo y)

instance (Monoid a) => Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo (\_ -> mempty)
