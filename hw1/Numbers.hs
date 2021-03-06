{-# LANGUAGE InstanceSigs #-}

module Numbers
  (
    Nat (Z, S)
  , add
  , div
  , fromInt
  , isEven
  , mod
  , mul
  , sub
  , toInt
  ) where

import Prelude hiding (div, mod)

-- | Number representation
data Nat
  = Z
  | S Nat
  deriving Show

instance Eq Nat where
  (==)
    :: Nat
    -> Nat
    -> Bool
  (==) Z Z         = True
  (==) Z _         = False
  (==) _ Z         = False
  (==) (S x) (S y) = x == y

instance Ord Nat where
  (<=)
    :: Nat
    -> Nat
    -> Bool
  (<=) Z _         = True
  (<=) _ Z         = False
  (<=) (S x) (S y) = x <= y

-- | Return the sum of two Nat
add
  :: Nat  -- ^ First addend
  -> Nat  -- ^ Second addend
  -> Nat  -- ^ The sum of input addends
add x Z     = x
add x (S y) = S (add x y)

-- | Return the quotient of two Nat
div
  :: Nat  -- ^ Dividend
  -> Nat  -- ^ Divisor
  -> Nat  -- ^ Quotient
div _ Z       = error "Division by zero is undefined"
div x y
  | x < y     = Z
  | otherwise = S $ div (sub x y) y

-- | Return Nat from Int
fromInt
  :: Int  -- ^ Input Int
  -> Nat  -- ^ The equal Nat from Int
fromInt x
  | x <= 0    = Z
  | otherwise = S $ fromInt $ x - 1

-- | Determine if Nat is even
isEven
  :: Nat   -- ^ Input Nat
  -> Bool  -- ^ True if Nat is even otherwise False
isEven Z     = True
isEven (S Z) = False
isEven num   = isEven $ sub num $ S $ S Z

-- | Return the remainder of two Nat
mod
  :: Nat  -- ^ Dividend
  -> Nat  -- ^ Divisor
  -> Nat  -- ^ Remainder
mod x y = sub x $ mul y $ div x y

-- | Return the product of two Nat
mul
  :: Nat  -- ^ First multiplier
  -> Nat  -- ^ Second multiplier
  -> Nat  -- ^ The product of input multipliers
mul _ Z     = Z
mul x (S y) = add x $ mul x y

--   | Return the subtract of two Nat
sub
  :: Nat  -- ^ Minuend
  -> Nat  -- ^ Subtrahend
  -> Nat  -- ^ The subtraction of input Nat
sub Z      _    = Z
sub num    Z    = num
sub (S x) (S y) = sub x y

-- | Return Int from Nat
toInt
  :: Nat  -- ^ Input Nat
  -> Int  -- ^ The equal Int from Nat
toInt Z     = 0
toInt (S x) = (toInt x) + 1
