{-# LANGUAGE InstanceSigs #-}

module BaseCombinators
  (
    ok
  , eof
  , satisfy
  , element
  , stream
  ) where

import CopyPaste (Parser (Parser))
import Data.List

-- | Parser, that always returns empty result and initial input
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

checkEof
  :: [s]
  -> Maybe ((), [s])
checkEof [] = Just ((), [])
checkEof _  = Nothing

-- | Parser, that accepts only eof, otherwise returns Nothing
eof :: Parser s ()
eof = Parser $ \s -> checkEof s

checkPredicate
  :: (s -> Bool)
  -> [s]
  -> Maybe (s, [s])
checkPredicate _ [] = Nothing
checkPredicate p (x : xs)
  | p x = Just (x, xs)
  | otherwise = Nothing

-- | Parser, that accept element s only if predicate is True on this symbol
-- otherwise returns Nothing
satisfy
  :: (s -> Bool)
  -> Parser s s
satisfy p = Parser $ \s -> checkPredicate p s

checkSymbol
  :: (Eq s) => s
  -> [s]
  -> Maybe (s, [s])
checkSymbol _ [] = Nothing
checkSymbol s (x : xs)
  | s == x = Just (s, xs)
  | otherwise = Nothing

-- | Parser, that accept only if symbols coincidences, otherwise returns
-- Nothing
element
  :: (Eq s) => s
  -> Parser s s
element s = Parser $ \s' -> checkSymbol s s'

checkString
  :: (Eq s) => [s]
  -> [s]
  -> Maybe ([s], [s])
checkString prefix str = stripPrefix prefix str >>= \suffix -> Just (prefix, suffix)

-- | Parser, that accept only if prefixes coincidence, otherwise returns
-- Nothing
stream
  :: (Eq s) => [s]
  -> Parser s [s]
stream s = Parser $ \s' -> checkString s s'
