{-# LANGUAGE InstanceSigs #-}

module HardParser
  (
    listlistParser
  ) where

import BaseCombinators (element, eof)
import Control.Applicative
import CopyPaste (Parser)
import SimpleParsers (parseInt)

skip :: Parser Char ()
skip = many (element ' ') >> return ()

comma :: Parser Char ()
comma = skip >> element ',' >> skip

getIntList
  :: Int
  -> Parser Char [Int]
getIntList 0 = return []
getIntList n = comma >> parseInt >>= \x -> getIntList (n - 1) >>= \xs -> return (x : xs)

getList :: Parser Char [Int]
getList = skip >> parseInt >>= \n -> getIntList n >>= \xs -> return xs

getLists :: Parser Char [[Int]]
getLists = (comma >> getList >>= \x -> getLists >>= \xs -> return (x : xs)) <|> (eof >> return [])

-- | Parser, that parsed list of lists, every first symbol is the length of current list
listlistParser :: Parser Char [[Int]]
listlistParser = (getList >>= \x -> getLists >>= \xs -> return (x : xs)) <|> (eof >> return [])
