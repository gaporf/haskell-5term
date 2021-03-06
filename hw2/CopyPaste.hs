{-# LANGUAGE InstanceSigs #-}

module CopyPaste
  (
    Parser (Parser, runParser)
  ) where

import Control.Applicative

-- | Implementation of parser, that get [s] and return value a and rest part [s]
newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

first
  :: (a -> b)
  -> (a, c)
  -> (b, c)
first f (x, c) = (f x, c)

instance Functor (Parser s) where
  fmap
    :: (a -> b)
    -> (Parser s a)
    -> (Parser s b)
  fmap f (Parser parse) = Parser $ (fmap (first f)) . parse

appFunc
  :: [s]
  -> ([s] -> Maybe (a -> b, [s]))
  -> ([s] -> Maybe (a, [s]))
  -> Maybe (b, [s])
appFunc s p1 p2 = do
  (a', s') <- (p1 s)
  (a'', s'') <- (p2 s')
  Just (a' a'', s'')

instance Applicative (Parser s) where
  pure
    :: a
    -> (Parser s a)
  pure a = Parser $ (\s -> Just (a, s))
  (<*>)
    :: (Parser s (a -> b))
    -> (Parser s a)
    -> (Parser s b)
  (Parser p1) <*> (Parser p2) = Parser (\s -> appFunc s p1 p2)

monFunc
  :: [s]
  -> ([s] -> Maybe (a, [s]))
  -> (a -> (Parser s b))
  -> Maybe (b, [s])
monFunc s p1 f = do
  (a, s') <- (p1 s)
  (b, s'') <- (runParser (f a) s')
  Just (b, s'')

instance Monad (Parser s) where
  return
    :: a
    -> (Parser s a)
  return a = Parser $ (\s -> Just (a, s))
  (>>=)
    :: (Parser s a)
    -> (a -> (Parser s b))
    -> (Parser s b)
  (Parser p1) >>= f = Parser (\s -> monFunc s p1 f)

altFunc
  :: [s]
  -> ([s] -> Maybe (a, [s]))
  -> ([s] -> Maybe (a, [s]))
  -> Maybe (a, [s])
altFunc s p1 p2 = do
  (a, s') <- (p1 s) <|> (p2 s)
  Just (a, s')

instance Alternative (Parser s) where
  empty :: (Parser s a)
  empty = Parser (\_ -> Nothing)
  (<|>)
    :: (Parser s a)
    -> (Parser s a)
    -> (Parser s a)
  (Parser p1) <|> (Parser p2) = Parser (\s -> altFunc s p1 p2)
