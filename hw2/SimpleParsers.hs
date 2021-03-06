{-# LANGUAGE InstanceSigs #-}

module SimpleParsers
  (
    balancedBrackets
  , parseInt
  ) where

import BaseCombinators (element, eof, ok, satisfy)
import Control.Applicative
import CopyPaste (Parser)
import Data.Char

brack :: Parser Char ()
brack = ((element '(') >> brack >> (element ')') >> brack) <|> ok

-- | Parser, that checks the string for balanced brackets sequence
balancedBrackets :: Parser Char ()
balancedBrackets = brack >> eof

-- | Parser, that check the string is numbers
parseInt :: Parser Char Int
parseInt = let s = (element '+' >> return 1) <|> (element '-' >> return (-1)) <|> (ok >> return 1)
           in s >>= \x -> some (satisfy isDigit) >>= \y -> return (x * (read y))
