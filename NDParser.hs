---------------
-- Section 1 --
---------------

module NDParser where

import Control.Applicative
import Control.Monad

-- The Non-Deterministic Parser Monad
-------------------------------------

-- A Parser of things
-- is a function from Strings
-- to lists of pairs
-- of things and Strings
data NDParser a = Parse (String -> [(a, String)])

-- Generating every (partial) String parse from an 'NDParser a' just
-- requires applying the function held within the 'Parse' constructor
-- to the input string.
everyParse :: NDParser a -> String -> [(a, String)]
everyParse (Parse f) s = f s

-- To get you started, here are implementations of the Functor,
-- Applicative, Monad, and Alternative APIs for NDParsers.
instance Functor NDParser where
  fmap f p = Parse (\s -> [ (f x, s') | (x, s') <- everyParse p s ])

instance Applicative NDParser where
  pure x  = Parse (\s -> [(x, s)])

  q <*> p = Parse (\s -> [ (f x, s'')
                         | (f,   s')  <- everyParse q s,
                           (x,   s'') <- everyParse p s' ])

instance Monad NDParser where
  return = pure

  p >>= h = Parse (\s -> [ (y, s'')
                         | (x, s')  <- everyParse p s,
                           (y, s'') <- everyParse (h x) s' ])

instance Alternative NDParser where
  empty = Parse (\s -> [])

  p1 <|> p2 = Parse (\s -> everyParse p1 s ++ everyParse p2 s)


-- Exercise 1.1
---------------

-- 'end' successfully returns a '()' value *only* at the end of the
-- input (that is, the input String is empty).  Otherwise, 'end' fails
-- with no possible parses on any other, non-empty input String.
end :: NDParser ()
end = Parse f
  where
    f "" = [((), "")]
    f _  = []

-- Hint: To implement 'end', look at the provided implementation of
-- 'next' below for inspiration.

-- 'next' returns the next character of the input String whenever the
-- input is not empty.  If the input String is empty, then 'next'
-- fails with no possible parses.
next :: NDParser Char
next = Parse (\s -> case s of
                      []   -> []
                      c:cs -> [(c, cs)])


-- Bonus Exercise 1.2
---------------------

-- 'feed' puts the given character in front of the input String, "feeding" it to
-- the next parser operation that executes.  'feed' does not read the input
-- string at all, and always succeeds for any input with a return value of '()'.
-- After 'feed c' finishes, the String left to be parsed will always be 1
-- character longer, starting with 'c' and followed by the original input
-- String.
feed :: Char -> NDParser ()
feed c = Parse f
  where
    f s = [((), c:s)]

-- Exercise 1.3
---------------

-- 'char c' successfully returns the next character of the input
-- String *only* if that next character is *exactly* equal to the
-- given c.  Otherwise, 'char c' will fail with no possible parses (in
-- the cases when the input String is empty, or when it starts with
-- any character not equal to the parameter c).
char :: Char -> NDParser Char
char c = check (== c)

-- Hint: See if you can implement 'char' by using the provided 'check'
-- parser below.

-- 'check f' successfully returns the next character of the input
-- String *only* if the given function f returns True for that
-- character.  Otherwise, 'check f' will fail with no possible parses
-- (in the cases where the input string is empty, or when it starts
-- with some character c such that 'f c' returns False).
check :: (Char -> Bool) -> NDParser Char
check f = do c <- next
             if (f c)
                then return c
                else empty


-- Exercise 1.4
---------------

-- 'digit' parses exactly one ASCII digit (one of the characters
-- between '0', '1', '2', ..., '9') from the start of the input.
digit :: NDParser Char
digit = oneOf ['0'..'9']

-- Hint: Look at the provided implementation of the 'space' parser
-- below for help with how to implement 'digit'.

-- 'digits' parses *one or more* digit characters from the start of
-- the input.
digits :: NDParser String
digits = some digit


-- Additional Functions
-----------------------

-- 'space' parses exactly one ASCII blank space character (which can
-- be any one of a single space ' ', a tab '\t', a new line '\n', or a
-- line feed '\f') from the start of the input.
space :: NDParser Char
space = oneOf [' ', '\t', '\n', '\f']

-- 'spaces' parses *zero or more* blank space characters from the
-- start of the input.
spaces :: NDParser String
spaces = many space

-- 'oneOf cs' successfully returns the next the next character of the
-- input String *only* if that next character matches any *one of* the
-- characters in the Char list cs.  Otherwise, 'oneOf cs' fails with
-- no possible parses (in the cases where the input String is empty,
-- or when the next character does not match any of the characters
-- found in the given list cs).
oneOf :: [Char] -> NDParser Char
oneOf []     = empty
oneOf (c:cs) = char c <|> oneOf cs
