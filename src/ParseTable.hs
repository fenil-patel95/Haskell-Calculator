---------------
-- Section 4 --
---------------

module ParseTable where

import Control.Applicative
import Data.Map (Map)
-- Imports the Map type and functions singleton, findWithDefault, and
-- unionsWith unqualified, so you can refer to them by just these
-- names
import qualified Data.Map as M
-- Imports these functions as M.empty and M.null to avoid name clash
-- with the existing empty and null from elsewhere

import GenericParser

-- The ParseTable Applicative Functor
-------------------------------------

-- The definition of parse tables as a concrete data structure.
data ParseTable a = Done a
                  | Fork [ParseTable a]
                  | Look (Map (Maybe Char) (ParseTable a))
                    deriving Show

-- Implementations of the Functor, Applicative, and Alternative APIs
-- for ParseTables.
instance Functor ParseTable where
  fmap f (Done x)     = Done (f x)
  fmap f (Fork ps)    = Fork [ fmap f p | p <- ps ]
  fmap f (Look table) = Look (fmap (fmap f) table)

instance Applicative ParseTable where
  pure x = Done x

  Fork qs    <*> p          = Fork [ q <*> p | q <- qs ]
  Look table <*> p          = Look (fmap (<*> p) table)
  q          <*> Fork ps    = Fork [ q <*> p | p <- ps ]
  q          <*> Look table = Look (fmap (q <*>) table)
  Done f     <*> Done x     = Done (f x)

instance Alternative ParseTable where
  empty = Fork []

  p1 <|> p2 = Fork [p1, p2]

-- There is no Monad implemenation of ParseTables.  Why?


-- Bonus Exercise 4.1
---------------------

parseTable :: ParseTable a -> String -> [(a, String)]
parseTable (Done x) s = [(x, s)]
parseTable (Fork ps) s = concat [parseTable p s | p <- ps]
parseTable (Look table) "" =
  case M.lookup Nothing table of
    Nothing -> []
    Just p  -> parseTable p ""
parseTable (Look table) (c:cs) =
  case M.lookup (Just c) table of
    Nothing -> []
    Just p  -> parseTable p cs

-- Bonus Exercise 4.2
---------------------

instance Parser ParseTable where
  end = Look (M.singleton Nothing (Done ()))
  char c = Look (M.singleton (Just c) (Done c))


-- Bonus Exercise 4.3
---------------------

immediate :: ParseTable a -> [a]
immediate (Done x) = [x]
immediate (Fork ps) = concatMap immediate ps
immediate (Look _) = []

lookahead :: ParseTable a -> Map (Maybe Char) (ParseTable a)
lookahead (Done _) = M.empty
lookahead (Look table) = table
lookahead (Fork ps) = M.unionsWith (<|>) [lookahead p | p <- ps]


-- Bonus Exercise 4.4
---------------------

inlineFork1 :: ParseTable a -> ParseTable a
inlineFork1 (Fork [p]) = p
inlineFork1 p = p

optimize :: ParseTable a -> ParseTable a
optimize p =
  let table = M.map optimize (lookahead p)
      dones = [Done x | x <- immediate p]
      result =
        if M.null table
          then Fork dones
          else Fork (dones ++ [Look table])
  in inlineFork1 result
