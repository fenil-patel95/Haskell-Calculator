---------------
-- Section 3 --
---------------

module GenericParser where

import Control.Applicative
import NDParser (NDParser)
import qualified NDParser as ND

-- A general API for Parsers
----------------------------

class (Alternative m) => Parser m where
  end  :: m ()
  char :: Char -> m Char


-- Bonus Exercise 3.1
---------------------

instance Parser NDParser where
  end = ND.end

  char c = ND.char c

-- Bonus Exercise 3.2
---------------------

string :: Parser m => String -> m String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs

-- Bonus Exercise 3.3
---------------------

digit :: Parser m => m Char
digit = oneOf ['0'..'9']

digits :: Parser m => m String
digits = some digit

space :: Parser m => m Char
space = oneOf [' ', '\t', '\n', '\f']

spaces :: Parser m => m String
spaces = many space

oneOf :: Parser m => [Char] -> m Char
oneOf [] = empty
oneOf (c:cs) = char c <|> oneOf cs