{-# LANGUAGE InstanceSigs #-}

module Moving
  (
    moving
  ) where

import Control.Monad.State

type Stack = [Float]

push
  :: Float
  -> State Stack ()
push x = state $ \xs -> ((), x : xs)

mov
  :: Int
  -> [Float]
  -> [Float]
  -> Float
  -> Float
  -> State Stack ()
mov _ [] _ _ _ = return ()
mov 0 (x : xs) (y : ys) s len = let s' = s - y + x
                                in (mov 0 xs ys s' len) >> push (s' / len)
mov n (x : xs) ys s len = let s' = s + x
                          in let len' = len + 1
                            in (mov (n - 1) xs ys s' len') >> push (s' / len')

-- | The first argument is n, the second is list, the return value is list
-- where i-th position contains mean of [max(0, i - n + 1), i]
moving
  :: Int
  -> [Float]
  -> [Float]
moving n xs = execState (mov n xs xs 0 0) []
