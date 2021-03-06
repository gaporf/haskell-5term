{-# LANGUAGE InstanceSigs #-}

module Concat
  (
    eitherConcat
  , maybeConcat
  ) where

help2
  :: Monoid m
  => Monoid n
  => Either m n
  -> (m, n)
  -> (m, n)
help2 (Left mx) (mp, np)  = (mx <> mp, np)
help2 (Right nx) (mp, np) = (mp, nx <> np)

-- | Return a pair of two elements Left and Right, got by monoid union
eitherConcat
  :: Monoid m
  => Monoid n
  => [Either m n]  -- ^ Input list of Either
  -> (m, n)        -- ^ Pair of two monoid union elements
eitherConcat list = foldr help2 (mempty, mempty) list

help
  :: Maybe [a]
  -> [a]
  -> [a]
help Nothing xs  = xs
help (Just x) xs = x ++ xs

-- | Return a list from concatenation of internal lists in Maybe
maybeConcat
  :: [Maybe [a]]  -- ^ Input list of Maybe
  -> [a]          -- ^ Concatenation of internal lists in Maybe
maybeConcat list = foldr help [] list
