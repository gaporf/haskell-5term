{-# LANGUAGE InstanceSigs #-}

module Split
  (
    joinWith
  , splitOn
  ) where

import Data.List.NonEmpty as NonEmpty

--  | Return a string, got by joining strings with special symbol. It is
--  necessary that the list of strings contains at least one string.
joinWith
  :: Char               -- ^ Input char to join adjacent strings in the list
  -> (NonEmpty String)  -- ^ Input list of strings to join
  -> String             -- ^ Final string after joining
joinWith separator parts = Prelude.tail $ foldr (\x xs -> separator : x ++ xs) "" parts

check
  :: Char
  -> Char
  -> (NonEmpty String)
  -> (NonEmpty String)
check separator char xs@(x :| xt)
  | separator == char = "" <| xs
  | otherwise         = (char : x) :| xt

--  | Return a list of strings, got by spliting initial string with given
--  separator. It is guaranteed that return list always contains at least
--  one element.
splitOn
  :: Char               -- ^ Input char to split
  -> String             -- ^ Input string to split
  -> (NonEmpty String)  -- ^ Nonempty list of strings
splitOn separator string = foldr (check separator) ("" :| []) string
