---------------
-- Section 2 --
---------------

module Calculate where

import Control.Applicative
-- To implement all the bonus exercises in Sections 3 and 4, you will
-- need to remove the "import NDParser" above, and uncomment the
-- following line which imports GenericParser in its place.

import GenericParser

import ParseTable


-- Exercise 2.1
---------------

-- 'naturalNumber' parses a single, natural number from the start of
-- the input String, which is read from one or more digit characters.
-- The value returned from a successful 'naturalNumber' parse is the
-- interpretation of that number as a Double-precision floating-point
-- number.  This number may be zero, or any positive whole number, but
-- will not be negative.
naturalNumber :: (Num a, Read a, Parser m) => m a
naturalNumber = read <$> digits

-- 'negativeNumber' parses a single negative number from the start of
-- the input String.  A negative number begins with a '-' sign,
-- followed by one or more digit characters.  The value returned from
-- a successful 'negativeNumber' parse is the negation of the (zero or
-- positive) 'naturalNumber' that follows the '-' sign.
negativeNumber :: (Num a, Read a, Parser m) => m a
negativeNumber = negate <$> (char '-' *> naturalNumber)

-- 'integer' parses EITHER a 'naturalNumber' OR a 'negativeNumber'
-- from the start of the input String.
integer :: (Num a, Read a, Parser m) => m a
integer = negativeNumber <|> naturalNumber


-- Bonus Exercise 2.2
---------------------

-- 'decimalFraction' parses the fractional part of a decimal-point-formatted
-- number following the '.' point.  'decimalFraction' reads a sequence of (one
-- or more) digits, and then returns a Double between 0 (inclusive) and 1
-- (exclusive).  For example, when 'decimalFraction' parses an input String
-- beginning with the digits "123456", it will return the fractional number
-- 0.123456, which is equal to 123456 / 10^6.
decimalFraction :: (Fractional a, Read a, Parser m) => m a
decimalFraction =
  (\ds -> read ("0." ++ ds)) <$> digits

-- 'float' parses a single floating-point number formatted as a sequence of:
--
--   1. an optional sign (the character '-' or nothing) followed by
--
--   2. the whole part (one or more digits) followed by
--
--   3. the decimal point (the character '.') followed by
--
--   4. the fractional part (one or more digits).
--
-- When successful, the returned result can be calculated by adding together the
-- whole and fractional parts, and then negating the result if a '-' sign was
-- read.
float :: (Fractional a, Read a, Parser m) => m a
float =
  (\w f -> w + f) <$> naturalNumber <*> (char '.' *> decimalFraction)
  <|>
  (\w f -> -(w + f)) <$> (char '-' *> naturalNumber) <*> (char '.' *> decimalFraction)
  
-- Hint: If you combine together steps 1 & 2 by calling the 'integer' parser
-- (which already looks for an optional '-' sign before parsing the number), be
-- careful when you calculate the total float!  If the whole number is negative,
-- then you need to make sure that the fractional part is also negative to
-- correctly add the two together.  For example, if we begin parsing "-3.25" as
-- a combination of -3 and 0.25, adding them together directly would give -2.75.
-- Instead, the right answer is gotten from -3 - 0.25 = -3.25.

-- 'number' parses any number which may be formatted as a whole integer (an
-- optional '-' followed by zero or more digits) or as a floating-point number
-- (an integer followed by '.' and zero or more digits).
number :: (Fractional a, Read a, Parser m) => m a
number = float <|> integer

-- Exercise 2.3
---------------

-- Multiplicative operators: times ('*') and divide ('/').
times, divide, multiplicative :: (Fractional a, Parser m) => m (a -> a -> a)

-- 'times' parses exactly a '*' character from the start of the input
-- String, then returns the Haskell multiplication function (*).
times = (*) <$ char '*'

-- 'divide' parses exactly a '/' character from the start of the input
-- String, then returns the Haskell floating-point division function
-- (/).
divide = (/) <$ char '/'

-- 'multiplicative' parses any multiplicative operator, which is
-- EITHER 'times' OR 'divide'
multiplicative = times <|> divide

-- Hint: For help, look at how the provided implementation of similar
-- parsers 'plus', 'minus', and 'additive' are defined below.

-- Additive operators: plus ('+') and minus ('-').
plus, minus, additive :: (Num a, Parser m) => m (a -> a -> a)

-- 'plus' parses exactly a '+' character from the start of the input
-- String, then returns the Haskell addition function (+).
plus = (+) <$ char '+'

-- 'minus' parses exactly a '-' character from the start of the input
-- String, then returns the Haskell addition function (-).
minus = (-) <$ char '-'

-- 'additive' parses any additive operator, which is EITHER 'plus' OR
-- 'minus'
additive = plus <|> minus


-- Bonus Exercise 2.4
---------------------

-- 'power' parses exactly a '^' character from the start of the input String,
-- then returns the Haskell floating-point exponentiation function (**).
power :: (Floating a, Parser m) => m (a -> a -> a)
power = (**) <$ char '^'

-- Exercise 2.5
---------------

-- 'trim' surrounds a given parser in optional blank spaces.  In other
-- words, 'trim' ignores any amount of blank spaces from the input
-- String before and after running the given parser.  The parser 'trim
-- p' will
--
--   (1) parse zero or more blank spaces from the start input String,
--
--   (2) run the given parser p and remember the result (call it x),
--
--   (3) parse zero or more blank spaces from the input String again,
--
--   (4) return the result (x) saved from step (2) as the final result.
trim :: Parser m => m a -> m a
trim p = spaces *> p <* spaces

-- Hint: For help with how to implement 'trim', check the provided
-- implementation of the similar parser combinator 'parenthesized'
-- below.

-- 'parenthesized' surrounds a given parser in parentheses and
-- optional blank spaces.  In other words, 'parenthesized' begins and
-- ends by parsing an open and closed parenthesis character, and in
-- the middle runs the given parser surrounded in any amount of
-- ignored blank spaces.  The parser 'parenthesized p' will:
--
--   (1) parse an open parenthesis character '(' from the start of the
--       input String,
--
--   (2) run the trimmed version of the parser p ('trim p') which
--       returns the same result as p but with any blank spaces before
--       or after ignored; the result returned by p (in the middle of
--       the blank spaces) is remembered (call it x)
--
--   (3) parse a close parenthesis character ')' from the input String,
--
--   (4) return the result (x) saved from step (2) as the final result.
parenthesized :: Parser m => m a -> m a
parenthesized p = char '(' *> trim p <* char ')'

-- Exercise 2.6
---------------

-- Hint: Do not change the type signatures of 'applyBinR', 'compose',
-- or 'applyL'!  If you fill in a return value for these functions
-- that type checks, then the answer will be correct, because the
-- types rule out any incorrect answers to this exercise.

-- 'applyBinR f y' partially applies the given function f to y as its
-- second argument.  In other words, 'applyBinR f y' takes the binary
-- function f (that is, f expects two arguments), and returns a
-- function expecting one argument x, which itself returns f applied
-- to x first and y second.
applyBinR :: (a -> b -> c) -> b -> (a -> c)
applyBinR f y = \x -> f x y

-- 'binopR op right' parses the partial application given by the 'op'
-- parser for a binary function applied to the 'right' expression as
-- its second argument.
binopR :: Parser m => m (a -> b -> c) -> m b -> m (a -> c)
binopR op right = applyBinR <$> trim op <*> right

-- 'compose f g' puts together the functions f and g by applying g to
-- the result of f, or in other words, passing the result of f to g.
-- 'compose f g' returns a function expecting one argument x, which
-- returns the composition g applied to f applied to x.
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \x -> g (f x)

-- Given that 'op' parses a binary operation and 'subexpr' parses a
-- sub-expression, 'chainR op subexpr' parses any number of partial
-- applications of 'op' to a 'subexpr' on its right, and composes them
-- all together so that the left-most operation goes first, then the
-- next one to the right, and so on.
chainR :: Parser m => m (a -> a -> a) -> m a -> m (a -> a)
chainR op subexpr =  compose <$> binopR op subexpr <*> chainR op subexpr
                 <|> pure (\x -> x)

-- 'applyL x f' takes an argument x and a function f expecting that
-- argument, and returns the result of f applied to x.
applyL :: a -> (a -> b) -> b
applyL x f = f x

-- 'assocL op subexpr' parses a left-associative chain of multiple
-- 'op' binary operations applied to sub-expressions parsed by
-- 'subexpr'.
assocL :: Parser m => m (a -> a -> a) -> m a -> m a
assocL op subexpr = applyL <$> subexpr <*> chainR op subexpr


-- Exercise 2.7
---------------

-- The three main types of arithmetic expressions, in terms of
-- operator precedence.
--
--   (1) Base expressions (baseExpr) include numbers and parenthesis
--
--   (2) Multiplication expressions (mulExpr) are chains of
--       multiplicative operations "times" ('*') or "divide" ('/')
--
--   (3) Addition expressions (addExpr) are chains of additive
--   operations "plus" ('+') or "minus" ('-')
baseExpr, mulExpr, addExpr :: (Floating a, Read a, Parser m) => m a

-- A base expression 'baseExpr' is EITHER an 'integer' OR a
-- 'parenthesized addExpr'.
baseExpr = parenthesized addExpr
       <|> function
       <|> number

-- A multiplicative expression 'mulExpr' is a left-associative chain
-- of 'multiplicative' operations, where the sub-expressions between
-- operators are base expressions ('baseExpr').
mulExpr = assocL multiplicative powExpr

-- An additive expression 'addExpr' is a left-associative chain of
-- 'additive' operations, where the sub-expressions between operators
-- are multiplicative expressions ('mulExpr').
addExpr  = assocL additive mulExpr

-- Hint: To implement 'mulExpr', look at how 'addExpr' is implemented.
-- You can parse the left-associative chain of multiplication
-- operations applying the 'assocL' parser combinator provided above
-- to the correct operator parser ('multiplicative') and the correct
-- sub-expression parser ('baseExpr').

function :: (Floating a, Read a, Parser m) => m a
function =
      (\x -> sqrt x) <$> (string "sqrt" *> parenthesized addExpr)
  <|> (\x -> abs x)  <$> (string "abs"  *> parenthesized addExpr)
  <|> (\x -> sin x)  <$> (string "sin"  *> parenthesized addExpr)
  <|> (\x -> cos x)  <$> (string "cos"  *> parenthesized addExpr)


-- Bonus Exercise 2.8
---------------------

-- An exponentiation expression 'powExpr' is a right-associative chain of
-- 'power' operations, where the sub-expressions between operators are base
-- expressions.
powExpr :: (Floating a, Read a, Parser m) => m a
powExpr = assocR power baseExpr

-- Hint 1: To implement 'powExpr', it will help to first implement the parser
-- combinators:

-- 'binop left op right' parses the left sub-expression first (getting a
-- returned value of type 'a'), second parses the binary operation 'a -> b -> c'
-- from 'op', third parses the right sub-expression (getting a returned value of
-- type 'b'), and finally returns a result of type 'c' by applying the operation
-- to values returned from the left and right sub-expressions.
binop :: Parser m => m a -> m (a -> b -> c) -> m b -> m c
binop left op right = (\x f y -> f x y) <$> left <*> trim op <*> right

-- 'assocR op subexpr' parses right associative applications of operators
-- returned by 'op and applied to the values of sub-expressions returned by
-- subexpr.  'assocR op subexpr' will be EITHER just a single 'subexpr' or the
-- 'binop' application of a single 'subexpr' followed by an 'op' and followed by
-- another right-associative 'assocR op subexpr'.
assocR :: Parser m => m (a -> a -> a) -> m a -> m a
assocR op subexpr = binop subexpr op (assocR op subexpr) <|> subexpr

-- Hint 2: To integrate 'powExpr' into the overall arithmetic parser, modify
-- 'mulExpr' from Exercise 2.7 to use 'powExpr' for its sub-expressions, rather
-- than 'baseExpr'.


-- The Top-level Expression Parser
----------------------------------

-- A top-level expression, 'expr' can be surrounded by black spaces
-- (which are ignored), and must consume the entire input String.
expr :: (Floating a, Read a, Parser m) => m a
expr = trim addExpr <* end

-- Bonus Exercise 4.5
---------------------

-- An optimized version of the 'expr' parser, based on ParseTables.
fastExpr :: ParseTable Double
fastExpr = optimize (expr :: ParseTable Double)