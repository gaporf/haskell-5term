{-# LANGUAGE InstanceSigs #-}

module NonEmpty
  (
    NonEmpty ((:|))
  ) where

-- | Implementation of the list with non-zero size
data NonEmpty a = a :| [a]
  deriving (Eq, Show)

conc
  :: NonEmpty a
  -> NonEmpty a
  -> NonEmpty a
conc (x :| xs) (y :| ys) = x :| (xs ++ y : ys)

join
  :: NonEmpty (NonEmpty a)
  -> NonEmpty a
join (x :| [])       = x
join (x :| (y : ys)) = conc x $ join (y :| ys)

instance Functor NonEmpty where
  fmap
    :: (a -> b)
    -> NonEmpty a
    -> NonEmpty b
  fmap f (x :| xs) = (f x) :| (fmap f xs)

instance Applicative NonEmpty where
  pure
    :: a
    -> NonEmpty a
  pure x = x :| []
  (<*>)
    :: NonEmpty (a -> b)
    -> NonEmpty a
    -> NonEmpty b
  (f :| fs) <*> (x :| xs) = (f x) :| ((fmap f xs) ++ (fs <*> xs) ++ [f' x | f' <- fs])

instance Monad NonEmpty where
  return
    :: a
    -> NonEmpty a
  return x = x :| []
  (>>=)
    :: NonEmpty a
    -> (a -> NonEmpty b)
    -> NonEmpty b
  xs >>= f = join $ fmap f xs

instance Foldable NonEmpty where
  foldr
    :: (a -> b -> b)
    -> b
    -> NonEmpty a
    -> b
  foldr f ini (x :| xs) = f x $ foldr f ini xs

instance Traversable NonEmpty where
  traverse
    :: Applicative f => (a -> f b)
    -> NonEmpty a
    -> f (NonEmpty b)
  traverse f (x :| xs) = (:|) <$> (f x) <*> (traverse f xs)
