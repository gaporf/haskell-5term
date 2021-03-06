{-# LANGUAGE InstanceSigs #-}

module SumInts
  (
    stringSum
  ) where

import Text.Read

-- | Sum integers in string, if there is non-integer substring, then return Nothing
stringSum
  :: String
  -> Maybe Int
stringSum nums = fmap sum $ sequenceA $ fmap readMaybe $ words nums
