{-# LANGUAGE BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor, StandaloneDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors -Wno-x-partial #-}

module Spec where

import NDParser (everyParse)
import qualified NDParser as ND
import Calculate
import GenericParser (Parser)
import qualified GenericParser as G
import ParseTable

import Data.Char
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.Hspec

test :: Spec
test = do
  context "Section 1: Parser Combinators" do
    context "Regular Exercise 1.1" do
      describe "NDParser.end" do
        it "successfully returns () when parsing the empty string" $
          everyParse ND.end "" `returns` [()]

        it "successfully leaves the empty string when parsing the empty string" $
          everyParse ND.end "" `leaves` [""]

        it "fails whenever given a non-empty string" $
          property \c s -> fails $ everyParse ND.end (c:s)

        it "is deterministic (always fails or returns 1 success)" $
          property \s -> deterministic $ everyParse ND.end s

    context "Bonus Exercise 1.2" do
      describe "NDParser.feed" do
        it "leaves an extra character in front of the remaining string to parse" $
          property \c s -> everyParse (ND.feed c) s `leaves` [c:s]

        it "'feed' followed by 'next' returns the fed character" do
          property \c s -> everyParse (ND.feed c >> ND.next) s `returns` [c]

        it "'feed' followed by 'next' leaves the input string as it was" do
          property \c s -> everyParse (ND.feed c >> ND.next) s `leaves` [s]

        it "'next' followed by 'feed' does nothing on non-empty input" do
          property \s ->
            not (null s)
            ==>
            everyParse (ND.next >>= ND.feed) s `parses` [((), s)]

        it "'next' followed by 'feed' fails when the input is empty" do
          fails $ everyParse (ND.next >>= ND.feed) ""

        it "always succeeds no matter the input string" $
          property \c s -> succeeds $ everyParse (ND.feed c) s

        it "is deterministic (always fails or returns 1 success)" $
          property \c s -> deterministic $ everyParse (ND.feed c)  s

    context "Regular Exercise 1.3" do
      describe "NDParser.char" do
        it "fails on the empty input" $
          property \c -> fails $ everyParse (ND.char c) ""

        it "fails when the next input character is different from expected" $
          property \c s ->
            not (null s) && c /= head s
            ==>
            fails $ everyParse (ND.char c) s

        it "succeeds and returns the given character when it is the next input" $
          property \c s -> everyParse (ND.char c) (c:s) `returnsOnly` c

        it "removes the first character from the input when it succeeds" $
          property \c s -> everyParse (ND.char c) (c:s) `leavesOnly` s

        it "is deterministic (always fails or returns 1 success)" $
          property (\c s -> deterministic $ everyParse (ND.char c) s)
          .&&.
          property (\c s -> deterministic $ everyParse (ND.char c) (c:s))

    context "Regular Exercise 1.4" do
      describe "NDParser.digit" do
        it "returns the next character of the input if it is '0' to '9'" $
          property \(Digit d) s -> everyParse ND.digit (d:s) `returnsOnly` d

        it "removes the next digit from the input string, leaving the rest" $
          property \(Digit d) s -> everyParse ND.digit (d:s) `leavesOnly` s

        it "fails if the next character in the input is not '0' to '9'" $
          property \s ->
            not (null s) && not (isDigit (head s))
            ==>
            fails $ everyParse ND.digit s

        it "fails on the empty input string" $
          fails $ everyParse ND.digit ""

        it "is deterministic (always fails or returns 1 success)" $
          property (\(Digit d) s -> deterministic $ everyParse ND.digit (d:s))
          .&&.
          property (\s ->
                      null s || not (isDigit (head s))
                      ==>
                      deterministic $ everyParse ND.digit s)

  context "Section 2: Arithmetic Expressions" do
    context "Regular Exercise 2.1" do
      describe "Calculate.naturalNumber" do
        it "fails on the empty input" $
          fails $ everyParse naturalNumber ""

        it "is the numeric value of 'digit' for just a single digit character" $
          property \(Digit d) ->
            everyParse naturalNumber [d]
            `parses`
            everyParse (ND.check isDigit >>= return . read . (:"")) [d]

        it "is the numeric value of 'digit' for a digit followed by a non-digit" $
          property \(Digit d) s ->
            not (null s) && not (isDigit (head s))
            ==>
            everyParse naturalNumber (d:s)
            `parses`
            everyParse (ND.check isDigit >>= return . read . (:"")) (d:s)

        it "can fully parse any sequence of digits as a natural number" $
          property \(Digits ds) ->
            not (null ds)
            ==>
            everyParse naturalNumber ds `returns` [read ds]

        it "correctly parses the shown representation of a non-negative integer" $
          property \(NonNegative n) ->
            everyParse (naturalNumber >>= return . round) (show n) `returns` [n :: Int]

        it "stops once it reaches a non-digit character" $
          property \(Digits ds) s ->
            not (null s) && not (isDigit (head s))
            ==>
            everyParse naturalNumber (ds ++ s) `stopsAt` s

      describe "Calculate.negativeNumber" do
        it "fails on the empty input" $
          fails $ everyParse negativeNumber ""

        it "fails on any input that doesn't begin with '-'" $
          property \s ->
            not (null s) && head s /= '-'
            ==>
            fails $ everyParse negativeNumber s

        it "fails on an input beginning with '-' followed by a non-digit" $
          property \s ->
            null s || not (isDigit (head s))
            ==>
            fails $ everyParse negativeNumber ('-':s)

        it "negates the value returned by running negativeNumber after a '-'" $
          property \(Digits ds) s ->
            everyParse negativeNumber ('-' : ds ++ s)
            `parsesExactly`
            [ (-n, r) | (n, r) <- everyParse naturalNumber (ds++s) ]

        it "fully parses the shown representation of any negative integer" $
          property \(Negative n) ->
            everyParse (negativeNumber >>= return . round) (show n) `returns` [n :: Int]

      describe "Calculate.integer" do
        it "fails on the empty input" $
          fails $ everyParse integer ""

        it "fails on the any input that doesn't begin with '-' or a digit" $
          property \s ->
            not (null s) && head s /= '-' && not (isDigit (head s))
            ==>
            fails $ everyParse integer s

        it "fails on any input that begins with '-' followed by a non-digit" $
          property \s ->
            null s || not (isDigit (head s))
            ==>
            fails $ everyParse integer ('-':s)

        it "fully parses the shown representation of *any* integer" $
          property \(n :: Int) ->
            everyParse (integer >>= return . round) (show n) `returns` [n]

        it "stops once it reaches a non-digit character (maybe after '-')" $
          property \(n :: Int) s ->
            not (null s) && not (isDigit (head s))
            ==>
            everyParse naturalNumber (show n ++ s) `stopsAt` s

    context "Bonus Exercise 2.2" do
      describe "Calculate.decimalFraction" do
        it "fails on the empty input" $
          fails $ everyParse decimalFraction ""

        it "is 'digit / 10' for just a single digit character" $
          property \(Digit d) ->
            everyParse decimalFraction [d]
            `parses`
            everyParse (ND.check isDigit >>= return . (/10) . read . (:"")) [d]

        it "is 'digit / 10' for a digit followed by anything else" $
          property \(Digit d) s ->
            not (null s) && not (isDigit (head s))
            ==>
            everyParse decimalFraction (d:s)
            `parses`
            everyParse (ND.check isDigit >>= return . (/10) . read . (:"")) (d:s)

        it "can fully parse any n-digit sequence as a fraction (over 10^n)" $
          property \(Digits ds) ->
            not (null ds)
            ==>
            let n      = length ds
                expect = read ds / (10 ^ n)
                fudge  = 1 / (10 ^ min (n-1) 15)
            in everyParse decimalFraction ds
               `returnSome`
               \n -> n >= expect - fudge .&&. n <= expect + fudge

        it "stops once it reaches a non-digit character" $
          property \(Digits ds) s ->
            not (null s) && not (isDigit (head s))
            ==>
            everyParse decimalFraction (ds ++ s) `stopsAt` s

      describe "Calculate.float" do
        it "is the same as decimalFraction for input starting with '0.'" $
          property \(Digits ds) s ->
            everyParse float ("0." ++ ds ++ s)
            `parsesExactly`
            everyParse decimalFraction (ds ++ s)

        it "is the negation of decimalFraction for input starting with '-0.'" $
          property \(Digits ds) s ->
            everyParse float ("-0." ++ ds ++ s)
            `parsesExactly`
            everyParse (decimalFraction >>= return . negate) (ds ++ s)

        it "is the same as naturalNumber for '.0' following some digits" $
          property \(Digits ds) s ->
            null s || not (isDigit (head s))
            ==>
            everyParse naturalNumber (ds ++ s)
            `parses`
            everyParse float (ds ++ ".0" ++ s)

        it "is the same as integer for '.0' following '-' and some digits" $
          property \(Digits ds) s ->
            null s || not (isDigit (head s))
            ==>
            everyParse integer ("-" ++ ds ++ s)
            `parses`
            everyParse float ("-" ++ ds ++ ".0" ++ s)

        it "fully parses any floating point number (up to +/-10^6)" $
          property $ forAll (choose (-10.0**6, 10.0**6)) \(n :: Double) ->
            let n' = n+1
            in everyParse float (show n') `returns` [n']

      describe "Calculate.number" do
        it "is the same as integer for input starting with a shown Int" $
          property \(n :: Int) s ->
            everyParse number (show n ++ s)
            `parses`
            everyParse integer (show n ++ s)

        it "is the same as 'float' for input starting with a shown Float" $
          property $ forAll (choose (-10.0**6, 10.0**6)) \(n :: Double) s ->
            let n' = n+1
            in everyParse number (show n' ++ s)
               `parses`
               everyParse float (show n' ++ s)

    context "Regular Exercise 2.3" do
      describe "Calculate.times" $ singleOperator times "*" (*)

      describe "Calculate.divide" $ singleOperator divide "/" (/)

      describe "Calculate.multiplicative" $
        multiOperator multiplicative [("*", (*)), ("/", (/))]

    context "Bonus Exercise 2.4" do
      describe "Calculate.power" $ singleOperator power "^" (**)

    context "Regular Exercise 2.5" do
      describe "Calculate.trim" do
        it "is the same as the given parser when there are no spaces" $
          property \(Letters cs) (Digits ds) ->
            everyParse (trim word <* ND.check isDigit) (cs ++ ds)
            `parsesExactly`
            everyParse (word <* ND.check isDigit) (cs ++ ds)

        it "skips blank spaces before the real text to parse" $
          property \(Spaces s) (Letters cs) (Digits ds) ->
            everyParse (trim word <* ND.check isDigit) (s ++ cs ++ ds)
            `parsesExactly`
            everyParse (word <* ND.check isDigit) (cs ++ ds)

        it "skips blank spaces after the real text to parse" $
          property \(Letters cs) (Spaces s) (Digits ds) ->
            everyParse (trim word <* ND.check isDigit) (cs ++ ds)
            `parsesExactly`
            everyParse (word <* ND.check isDigit) (cs ++ ds)

        it "skips blank spaces before *and* after the real text to parse" $
          property \(Spaces s1) (Letters cs) (Spaces s2) (Digits ds) ->
            everyParse (trim word <* ND.check isDigit) (s1 ++ cs ++ s2 ++ ds)
            `parsesExactly`
            everyParse (word <* ND.check isDigit) (cs ++ ds)            

    context "Regular Exercise 2.6" do
      describe "Calculate.applyBinR" do
        it "passes a value as the 'right' (i.e., 2nd) argument to the function" $
          property \(f :: Fun (A, B) C) b a ->
            applyBinR (applyFun2 f) b a `shouldBe` applyFun2 f a b

      describe "Calculate.compose" do
        it "calls the 1st function, then passes its result to the 2nd function" $
          property \(f :: Fun A B) (g :: Fun B C) a ->
            compose (applyFun f) (applyFun g) a `shouldBe` applyFun g (applyFun f a)

      describe "Calculate.applyL" do
        it "passes the value to the function and returns its result" $
          property \a (f :: Fun A B) ->
            applyL a (applyFun f) `shouldBe` applyFun f a

    context "Regular Exercise 2.7" do
      describe "Calculate.baseExpr" do
        it "can parse integers with or without parentheses" $
          property \(b :: BaseExpr Int Int) ->
            everyParse baseExpr (show b) `returns` [eval b]

        it "parses EITHER an additive expression in parentheses or an integer" $
          property \(b :: BaseExpr Int (RegularExpr' Int)) ->
            let n = eval b
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse baseExpr (show b) `returns` [n]

      describe "Calculate.addExpr" do
        it "can parse sums (+ or -) of integers" $
          property \(s :: AddExpr Int) ->
            let n = eval s
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse addExpr (show s) `returns` [n]

        it "parses a left-associated summation of multiplicative expressions" $
          property \(b :: RegularExpr' Int) ->
            let n = eval b
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse addExpr (show b) `returns` [n]

        it "parses a left-associated summation of nested expressions" $
          property \(b :: RegularExpr' (RegularExpr' Int)) ->
            let n = eval b
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse addExpr (show b) `returns` [n]

      describe "Calculate.mulExpr" do
        it "can parse products (* or /) of integers" $
          property \(m :: MulExpr Int) ->
            let n = eval m
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse mulExpr (show m) `returns` [eval m]

        it "parses a left-associated product of base expressions" $
          property \(m :: MulExpr (BaseExpr Int Int)) ->
            let n = eval m
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse mulExpr (show m) `returns` [n]

        it "parses a left-associated product of nested expressions" $
          property \(m :: MulExpr (BaseExpr Int (RegularExpr' Int))) ->
            let n = eval m
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse mulExpr (show m) `returns` [n]

      describe "Calculate.expr" do
        it "uniquely parses arbitrarily nested expressions" $
          property \(x :: RegularExpr) ->
            let n = eval x
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse expr (show x) `returnsOnly` n

    context "Bonus Exercise 2.8" do
      describe "Calculate.powExpr" do
        it "can parse exponential towers of integers" $
          property \(e :: PowExpr Int) ->
            let n = eval e
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse powExpr (show e) `returns` [n]

        it "can parse right-assicated exponents of expressions" $
          property \(e :: PowExpr (BaseExpr Int (BonusExpr' Int))) ->
            let n = eval e
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse powExpr (show e) `returns` [n]

      describe "Calculate.expr" do
        it "uniquely parses arbitrarily nested expressions (with exponents)" $
          property \(x :: BonusExpr) ->
            let n = eval x
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse expr (show x) `returnsOnly` n

  context "Bonus Section 3: Parser API" do
    context "Bonus Exercise 3.1" do
      describe "Parser NDParser => GenericParser.end" do
        it "successfully returns () when parsing the empty string" $
          everyParse G.end "" `returns` [()]

        it "successfully leaves the empty string when parsing the empty string" $
          everyParse G.end "" `leaves` [""]

        it "fails whenever given a non-empty string" $
          property \c s -> fails $ everyParse G.end (c:s)

        it "is deterministic (always fails or returns 1 success)" $
          property \s -> deterministic $ everyParse G.end s

        it "is exactly the same as NDParser.end for every input" do
          property \s -> everyParse G.end s `parsesExactly` everyParse ND.end s

      describe "Parser NDParser => GenericParser.char" do
        it "fails when parsing the empty string" $
          property \c -> fails $ everyParse (G.char c) ""

        it "succeeds when the input starts with the given character" $
          property \c s -> everyParse (G.char c) (c:s) `returnsOnly` c

        it "fails when the input starts with the any other character" $
          property \c c' s ->
            c /= c'
            ==>
            fails $ everyParse (G.char c) (c':s)

        it "always leaves everything after the first character when it succeeds" $
          property \c s -> everyParse (G.char c) (c:s) `leavesOnly` s

        it "is deterministic (always fails or returns 1 success)" $
          (property \c s ->
              null s || head s /= c
              ==>
              deterministic $ everyParse (G.char c) s)
          .&&.
          (property \c s -> deterministic $ everyParse (G.char c) (c:s))

        it "is exactly the same as NDParser.char for every input" do
          property \c s ->
            everyParse (G.char c) s `parsesExactly` everyParse (ND.char c) s

    context "Bonus Exercise 3.2" do
      describe "GenericParser.string" do
        it "succeeds when parsing the empty string in the empty input" $
          property $ everyParse (G.string "") "" `returnsOnly` ""

        it "fails when parsing a non-empty string in the empty input" $
          property \w ->
            not (null w)
            ==>
            fails $ everyParse (G.string w) ""

        it "succeeds when the input starts with *exactly* the given string" $
          property \w s -> everyParse (G.string w) (w ++ s) `returnsOnly` w

        it "fails when the input starts with the any other string" $
          property \w s ->
            w /= take (length w) s
            ==>
            fails $ everyParse (G.string w) s

        it "fails even with the input starts with a portion of the string" $
          property \w1 w2 s ->
            let w = w1 ++ w2
                n = length w
                t = w1 ++ s
            in w /= take n t
               ==>
               fails $ everyParse (G.string w) t

        it "always leaves everything after the given string when it succeeds" $
          property \w s -> everyParse (G.string w) (w ++ s) `leavesOnly` s

        it "is deterministic (always fails or returns 1 success)" $
          (property \w s -> deterministic $ everyParse (G.string w) s)

    context "Bonus Exercise 3.3" do
      describe "GenericParser.oneOf" do
        it "works for any instance of Parser" $
          (oneOf' :: [Char] -> PParser Char) `seq` True

        it "returns the next character of the input is one of the given ones" $
          property \cs n s ->
            not (null cs)
            ==>
            let c = cs !! (n `mod` length cs)
            in everyParse (oneOf' cs) (c:s) `returnAlways` (== c)

        it "on success, removes the character from the input, leaving the rest" $
          property \cs n s ->
            not (null cs)
            ==>
            let c = cs !! (n `mod` length cs)
            in everyParse (oneOf' cs) (c:s) `leavesAlways` (== s)

        it "fails if the next input character is not one of the given ones" $
          property \cs s ->
            not (null cs) && not (null s) && head s `notElem` cs
            ==>
            fails $ everyParse (oneOf' cs) s

        it "fails on the empty input string" $
          property \cs -> not (null cs) ==> fails $ everyParse (oneOf' cs) ""

        it "is deterministic (when given distinct choices)" $
          property \cs s -> deterministic $ everyParse (oneOf' (nub cs)) s

      describe "GenericParser.space" do
        it "works for any instance of Parser" $
          (space' :: PParser Char) `seq` True

        it "returns the next character of the input if it is a space" $
          property \s -> everyParse space' (' ':s) `returnsOnly` ' '

        it "removes the next space from the input string, leaving the rest" $
          property \s -> everyParse space' (' ':s) `leavesOnly` s

        it "fails if the next character in the input is not a space" $
          property \s ->
            not (null s) && not (isSpace (head s))
            ==>
            fails $ everyParse space' s

        it "fails on the empty input string" $
          fails $ everyParse space' ""

        it "is deterministic (always fails or returns 1 success)" $
          property (\s -> deterministic $ everyParse space' (' ':s))
          .&&.
          property (\s ->
                      null s || not (isSpace (head s))
                      ==>
                      deterministic $ everyParse space' s)

      describe "GenericParser.spaces" do
        it "works for any instance of Parser" $
          (spaces' :: PParser String) `seq` True

        it "succeeds on the empty input" $
          succeeds $ everyParse spaces' ""

        it "succeeds when the input is just a single space character" $
          property $ everyParse spaces' " " `returns` [" "]

        it "succeeds when the input is a single space followed by anything else" $
          property \s ->
            not (null s) && not (isSpace (head s))
            ==>
            everyParse spaces' (' ':s) `parses` [(" ", s)]

        it "can fully parse any sequence of spaces" $
          property \n ->
          let ws = replicate n ' '
          in everyParse spaces' ws `returns` [ws]

        it "stops once it reaches a non-space character" $
          property \n s ->
          let ws = replicate n ' '
          in not (null s) && not (isSpace (head s))
             ==>
             everyParse spaces' (ws ++ s) `stopsAt` s

      describe "GenericParser.digit" do
        it "works for any instance of Parser" $
          (digit' :: PParser Char) `seq` True

        it "returns the next character of the input if it is '0' to '9'" $
          property \(Digit d) s -> everyParse digit' (d:s) `returnsOnly` d

        it "removes the next digit from the input string, leaving the rest" $
          property \(Digit d) s -> everyParse digit' (d:s) `leavesOnly` s

        it "fails if the next character in the input is not '0' to '9'" $
          property \s ->
            not (null s) && not (isDigit (head s))
            ==>
            fails $ everyParse digit' s

        it "fails on the empty input string" $
          fails $ everyParse digit' ""

        it "is deterministic (always fails or returns 1 success)" $
          property (\(Digit d) s -> deterministic $ everyParse digit' (d:s))
          .&&.
          property (\s ->
                      null s || not (isDigit (head s))
                      ==>
                      deterministic $ everyParse digit' s)

      describe "GenericParser.digits" do
        it "works for any instance of Parser" $
          (digits' :: PParser String) `seq` True

        it "fails on the empty input" $
          fails $ everyParse digits' ""

        it "succeeds when the input is just a single digit character" $
          property \(Digit d) ->
            everyParse digits' [d] `returnsOnly` [d]

        it "succeeds when the input is a single digit followed by anything else" $
          property \(Digit d) s ->
            not (null s) && not (isDigit (head s))
            ==>
            everyParse digits' (d:s) `parsesExactly` [([d], s)]

        it "can fully parse any sequence of digits" $
          property \(Digits ds) ->
            not (null ds)
            ==>
            everyParse digits' ds `returns` [ds]

        it "stops once it reaches a non-digit character" $
          property \(Digits ds) s ->
            not (null s) && not (isDigit (head s))
            ==>
            everyParse digits' (ds ++ s) `stopsAt` s

    context "Bonus Exercise 3.4" do
      describe "Calculate" do
        it "naturalNumber has been generalized to use the 'Parser m' interface" $
          (naturalNumber :: (Num a, Read a) => ParseTable a)
          `seq` True
        it "negativeNumber has been generalized to use the 'Parser m' interface" $
          (negativeNumber :: (Num a, Read a) => ParseTable a)
          `seq` True
        it "integer has been generalized to use the 'Parser m' interface" $
          (integer :: (Num a, Read a) => ParseTable a)
          `seq` True
        it "plus has been generalized to use the 'Parser m' interface" $
          (plus :: (Num a) => ParseTable (a -> a -> a))
          `seq` True
        it "minus has been generalized to use the 'Parser m' interface" $
          (minus :: (Num a) => ParseTable (a -> a -> a))
          `seq` True
        it "additive has been generalized to use the 'Parser m' interface" $
          (additive :: (Num a) => ParseTable (a -> a -> a))
          `seq` True
        it "times has been generalized to use the 'Parser m' interface" $
          (times :: (Fractional a) => ParseTable (a -> a -> a))
          `seq` True
        it "divide has been generalized to use the 'Parser m' interface" $
          (divide :: (Fractional a) => ParseTable (a -> a -> a))
          `seq` True
        it "multiplicative has been generalized to use the 'Parser m' interface" $
          (multiplicative :: (Fractional a) => ParseTable (a -> a -> a))
          `seq` True
        it "trim has been generalized to use the 'Parser m' interface" $
          (trim :: ParseTable a -> ParseTable a)
          `seq` True
        it "parenthesized has been generalized to use the 'Parser m' interface" $
          (parenthesized :: ParseTable a -> ParseTable a)
          `seq` True
        it "binopR has been generalized to use the 'Parser m' interface" $
          (binopR :: ParseTable (a -> b -> c) -> ParseTable b -> ParseTable (a -> c))
          `seq` True
        it "chainR has been generalized to use the 'Parser m' interface" $
          (chainR :: ParseTable (a -> a -> a) -> ParseTable a -> ParseTable (a -> a))
          `seq` True
        it "assocL has been generalized to use the 'Parser m' interface" $
          (assocL :: ParseTable (a -> a -> a) -> ParseTable a -> ParseTable a)
          `seq` True
        it "baseExpr has been generalized to use the 'Parser m' interface" $
          (baseExpr :: (Floating a, Read a) => ParseTable a)
          `seq` True
        it "mulExpr has been generalized to use the 'Parser m' interface" $
          (mulExpr :: (Floating a, Read a) => ParseTable a)
          `seq` True
        it "addExpr has been generalized to use the 'Parser m' interface" $
          (addExpr :: (Floating a, Read a) => ParseTable a)
          `seq` True
        it "decimalFraction has been generalized to use the 'Parser m' interface" $
          (decimalFraction :: (Fractional a, Read a) => ParseTable a)
          `seq` True
        it "float has been generalized to use the 'Parser m' interface" $
          (float :: (Fractional a, Read a) => ParseTable a)
          `seq` True
        it "number has been generalized to use the 'Parser m' interface" $
          (number :: (Fractional a, Read a) => ParseTable a)
          `seq` True
        it "power has been generalized to use the 'Parser m' interface" $
          (power :: (Floating a) => ParseTable (a -> a -> a))
          `seq` True
        it "binop has been generalized to use the 'Parser m' interface" $
          (binop :: ParseTable a -> ParseTable (a -> b -> c) -> ParseTable b -> ParseTable c)
          `seq` True
        it "assocR has been generalized to use the 'Parser m' interface" $
          (assocR :: ParseTable (a -> a -> a) -> ParseTable a -> ParseTable a)
          `seq` True
        it "powExpr has been generalized to use the 'Parser m' interface" $
          (powExpr :: (Floating a, Read a) => ParseTable a)
          `seq` True

  context "Bonus Section 4: Parse Tables" do
    context "Bonus Exercise 4.1" do
      describe "ParseTable (string \"aba\" <|> string \"abb\" <|> string \"abc\")" do
        it "parses the string \"aba\" followed by anything" do
          property \s ->
            parseTable abcUnoptimized ("aba" ++ s) `parsesOnly` ("aba", s)

        it "parses the string \"abb\" followed by anything" do
          property \s ->
            parseTable abcUnoptimized ("abb" ++ s) `parsesOnly` ("abb", s)

        it "parses the string \"abc\" followed by anything" do
          property \s ->
            parseTable abcUnoptimized ("abc" ++ s) `parsesOnly` ("abc", s)

        it "fails for any other input" do
          property \s ->
            take 3 s `notElem` ["aba", "abb", "abc"]
            ==>
            fails $ parseTable abcUnoptimized s

      describe "ParseTable.parseTable (Done x)" do
        it "always succeeds on any input string" $
          property \(x :: A) s -> succeeds $ parseTable (Done x) s

        it "always returns x regardless of input string" $
          property \(x :: A) s -> parseTable (Done x) s `returnsOnly` x

        it "leaves the input string alone" $
          property \(x :: A) s -> parseTable (Done x) s `leavesOnly` s

      describe "ParseTable.parseTable (Fork ps)" do
        it "combines the possible returned values from a list of Done parsers" $
          property \(xs :: [A]) s ->
            parseTable (Fork (map Done xs)) s `returnsExactly` xs

        it "always leaves the input string alone when each parser in `ps` does" $
          property \(xs :: [A]) s ->
            parseTable (Fork (map Done xs)) s `leavesExactly` replicate (length xs) s

        it "always fails when `ps` is empty" $
          property \s -> fails $ parseTable (Fork []) s

        it "is the same as `p` when `ps = [p]`" $
          property \(AlphaParse p :: AlphaParseTable A) (Alphas s) ->
            parseTable (Fork [p]) s `parsesExactly` parseTable p s

        it "combines the results of `p1` and `p2` when `ps = [p1, p2]`" $
          property \(AlphaParse p1) (AlphaParse p2) (Alphas s) ->
            parseTable (Fork [p1, p2] :: ParseTable A) s
            `parsesExactly`
            (parseTable p1 s ++ parseTable p2 s)

        it "fails on any input where every parser in `ps` fails" $
          property \as (Alphas s) ->
            let failures = [p :: ParseTable A
                           | AlphaParse p <- as, fails $ parseTable p s]
            in not (null failures)
               ==>
               fails $ parseTable (Fork failures) s

        it "succeeds on any input where just one parser in `ps` succeeds" $
          property \as (Alphas s) ->
            let ps = [p :: ParseTable A | AlphaParse p <- as]
            in or [succeeds $ parseTable p s | p <- ps]
               ==>
               succeeds $ parseTable (Fork ps) s

        it "combines all returned results from multiple successful parsers" $
          property \as (Alphas s) ->
            let ps = [p :: ParseTable A | AlphaParse p <- as]
                successful = [p | p <- ps, succeeds $ parseTable p s]
            in length successful >= 2
               ==>
               parseTable (Fork ps) s
               `parsesExactly`
               concat [parseTable p s | p <- successful]

      describe "ParseTable.parseTable (Look table)" do
        it "is deterministic when all the parsers in `table` are" $
          property \(answers :: [(Maybe AlphaChar, A)]) (Alphas as) ->
          let table = M.fromList [(getAlpha <$> c, Done x) | (c, x) <- answers]
          in deterministic $ parseTable (Look table) as

        it "always fails when the `table` is empty" $
          property \(Alphas as) ->
            fails $ parseTable (Look M.empty) as

        it "succeeds on the empty input if `table` has a `Nothing` value" $
          property \(AlphaTable table) (x :: A) ->
            let table' =  M.insert Nothing (Done x) table
            in parseTable (Look table') "" `parsesOnly` (x, "")

        it "reads the first character of the input if it is Just in the table" $
          property \(AlphaTable table) (Alpha a) (x :: A) (Alphas as) ->
            let table' = M.insert (Just a) (Done x) table
            in parseTable (Look table') (a : as) `parsesOnly` (x, as)

        it "fails when the first character of the input is not in the table" $
          property \(AlphaTable table :: AlphaTable A) (Alphas as) ->
            not (null as)
            ==>
            case M.lookup (Just $ head as) table of
              Nothing -> fails $ parseTable (Look table) as
              Just _  -> discard

        it "is the same as the parser associated with the first input character" $
          property \(AlphaTable table :: AlphaTable A)
                    (Alpha a)
                    (AlphaParse p)
                    (Alphas as) ->
            let table' = M.insert (Just a) p table
            in parseTable (Look table') (a:as) `parsesExactly` parseTable p as

    context "Bonus Exercise 4.2" do
      describe "Parser ParseTable => GenericParser.end" do
        it "successfully returns () when parsing the empty string" $
          parseTable G.end "" `returns` [()]

        it "successfully leaves the empty string when parsing the empty string" $
          parseTable G.end "" `leaves` [""]

        it "fails whenever given a non-empty string" $
          property \c s -> fails $ parseTable G.end (c:s)

        it "is deterministic (always fails or returns 1 success)" $
          property \s -> deterministic $ parseTable G.end s

        it "is the same as the NDParser instance" $
          property \s ->
            parseTable G.end s `parsesExactly` everyParse G.end s

      describe "Parser ParseTable => GenericParser.char" do
        it "fails when parsing the empty string" $
          property \c -> fails $ parseTable (G.char c) ""

        it "succeeds when the input starts with the given character" $
          property \c s -> parseTable (G.char c) (c:s) `returnsOnly` c

        it "fails when the input starts with the any other character" $
          property \c c' s ->
            c /= c'
            ==>
            fails $ parseTable (G.char c) (c':s)

        it "always leaves everything after the first character when it succeeds" $
          property \c s -> parseTable (G.char c) (c:s) `leavesOnly` s

        it "is deterministic (always fails or returns 1 success)" $
          (property \c s ->
              null s || head s /= c
              ==>
              deterministic $ parseTable (G.char c) s)
          .&&.
          (property \c s -> deterministic $ parseTable (G.char c) (c:s))

        it "is exactly the same as NDParser instance" do
          property \c s ->
            parseTable (G.char c) s `parsesExactly` everyParse (G.char c) s

    context "Bonus Exercise 4.3" do
      describe "ParseTable.immediate" do
        it "returns exactly one immediate result [x] for Done x" $
          property \(x :: A) ->
            immediate (Done x) `shouldBe` [x]

        it "is always empty [] for a Look" $
          property \(AlphaTable table :: AlphaTable A) ->
            immediate (Look table) `shouldBe` []

        it "combines all the immediate results of the parsers in a Fork" $
          property \(ps :: [AlphaParseTable A]) ->
            let ps' = getAlphaParse <$> ps
            in immediate (Fork ps') `shouldMatchList` [x | p <- ps', x <- immediate p]

      describe "ParseTable.lookahead" do
        it "returns an empty table for a Done parser" $
          property \(x :: A) ->
            lookahead (Done x) `shouldBe` M.empty

        it "returns the table inside Look, as-is" $
          property \(AlphaTable table :: AlphaTable A) ->
            lookahead (Look table) `shouldBe` table

        it "returns the union of all lookahead results from a Fork" $
          property \(ps :: [AlphaParseTable A]) ->
            let ps' = take 10 $ getAlphaParse <$> ps
            in lookahead (Fork ps')
               `shouldBe`
               M.unionsWith (<|>) [ lookahead p | p <- ps' ]
          

    context "Bonus Exercise 4.4" do
      describe "ParseTable.inlineFork1" do
        it "can simplify just one `Fork [p]` to `p` at the top of the parser" $
          property \(AlphaParse p :: AlphaParseTable A) ->
            inlineFork1 (Fork [p]) `shouldBe` p

        it "leaves any other form of parser alone" $
          property \(AlphaParse p :: AlphaParseTable A) ->
            case p of
              Fork [p'] -> discard
              _ -> inlineFork1 p `shouldBe` p


      describe "ParseTable.optimize" do
        it "correctly optimizes string \"aba\" <|> string \"abb\" <|> string \"abc\"" $
          optimize abcUnoptimized `shouldBe` abcOptimized

        it "never gives a parser with a Fork of one option" $
          property \(AlphaParse p :: AlphaParseTable A) -> noFork1 (optimize p)

        it "never gives a parser with a Fork of a Fork" $
          property \(AlphaParse p :: AlphaParseTable A) -> noForkFork (optimize p)

        it "never gives a parser with a Fork containing two Look tables" $
          property \(AlphaParse p :: AlphaParseTable A) -> noForkedLook (optimize p)

        it "is idempotent (optimizing twice is the same as optimizing once)" $
          property \(AlphaParse p :: AlphaParseTable A) ->
            let p' = optimize p
            in optimize p' `shouldBe` p'

        it "correctly preserves the behavior of parser failures" $
          property \(AlphaParse p :: AlphaParseTable A) (Alphas as) ->
            fails (parseTable p as)
            ==>
            fails (parseTable (optimize p) as)

        it "correctly preserves the behavior of successful parsing" $
          property \(AlphaParse p :: AlphaParseTable A) (Alphas as) ->
            let expected = parseTable p as
            in succeeds expected
               ==>
               parseTable (optimize p) as `parsesExactly` expected

    context "Bonus Exercise 4.5" do
      describe "Calculate.fastExpr" do
        it "is optimized" $
          isOptimized (pruneParseTable 0 4 fastExpr)

        it "uniquely parses arbitrarily nested expressions" $
          property \(x :: RegularExpr) ->
            let n = eval x
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse expr (show x) `returnsOnly` n

        it "uniquely parses arbitrarily nested expressions (with exponents)" $
          property \(x :: BonusExpr) ->
            let n = eval x
            in not (isNaN n) && not (isInfinite n)
               ==>
               everyParse expr (show x) `returnsOnly` n

fails         = null
succeeds      = not . fails
deterministic = (<= 1) . length

singleOperator :: ND.NDParser (Double -> Double -> Double)
               -> String -> (Double -> Double -> Double)
               -> SpecWith ()
singleOperator p name op = multiOperator p [(name, op)]

multiOperator :: ND.NDParser (Double -> Double -> Double)
              -> [(String, Double -> Double -> Double)]
              -> SpecWith ()
multiOperator p table = do
  forM_ table \(name, _) -> do
    it ("succeeds when parsing '" ++ name ++ "'") $
      succeeds $ everyParse p name

  it "fails when parsing anything else" $
    property \s ->
      null s || and [ head s /= head name | (name, _) <- table ]
      ==>
      fails $ everyParse p s

  it "is deterministic (always fails or returns 1 success)" $
    conjoin
    [ property (\s -> deterministic $ everyParse p (name++s))
    | (name, _) <- ("", undefined):table ]

  forM_ table \(name, _) -> do
    it ("leaves everything following '" ++ name ++ "'") $
      property \s -> everyParse p (name ++ s) `stopsAt` s

  forM_ table \(name, op) -> do
    it ("returns the Haskell function matching '" ++ name ++ "'") $
      property \x y ->
      let z = op x y
      in not (isNaN z) && not (isInfinite z)
         ==>
         conjoin [ op' x y `shouldBe` z | (op', _) <- everyParse p name ]


ans     `parses`        options = ans `shouldContain` options
ans     `parsesExactly` options = ans `shouldMatchList` options
[(x,r)] `parsesOnly`    (y,s)   = (x,r) `shouldBe` (y,s)
ans     `parsesOnly`    (y,s)   = ans `shouldBe` [(y,s)]

ans     `returns`        vals = map fst ans `shouldContain` vals
ans     `returnsExactly` vals = map fst ans `shouldMatchList` vals
[(x,_)] `returnsOnly`    y    = x `shouldBe` y
ans     `returnsOnly`    y    = map fst ans `shouldBe` [y]
ans     `returnAlways`   p    = conjoin [ p x | (x, _) <- ans ]
ans     `returnSome`     p    = disjoin [ p x | (x, _) <- ans ]

ans     `leaves`        strs = map snd ans `shouldContain` strs
ans     `leavesExactly` strs = map snd ans `shouldMatchList` strs
[(_,r)] `leavesOnly`    s    = r `shouldBe` s
ans     `leavesOnly`    s    = map snd ans `shouldBe` [s]
ans     `leavesAlways`  p    = conjoin [ p s | (_, s) <- ans ]
ans     `leavesSome`    p    = disjoin [ p s | (_, s) <- ans ]

ans `stopsAt` s = conjoin [ r `shouldEndWith` s | (_,r) <- ans ]

word = some (ND.check isAlpha)

oneOf'  :: Parser m => [Char] -> m Char
oneOf' = G.oneOf

space'  :: Parser m => m Char
space' = G.space

spaces' :: Parser m => m String
spaces' = G.spaces

digit'  :: Parser m => m Char
digit' = G.digit

digits' :: Parser m => m String
digits' = G.digits


newtype PParser a = PParse (String -> Maybe (a, String))

instance Functor PParser where
  fmap f (PParse px) = PParse (\s -> do (x, s') <- px s
                                        return (f x, s'))

instance Applicative PParser where
  pure x = PParse (\s -> return (x, s))

  PParse pf <*> PParse px =
    PParse (\s0 -> do (f, s1) <- pf s0
                      (x, s2) <- px s1
                      return (f x, s2))

instance Monad PParser where
  PParse px >>= f = PParse (\s0 -> do (x, s1) <- px s0
                                      let PParse py = f x
                                      py s1)

instance Alternative PParser where
  empty = PParse (\s -> Nothing)

  PParse p1 <|> PParse p2 = PParse (\s -> p1 s <|> p2 s)

instance Parser PParser where
  end = PParse (\s -> case s of
                   "" -> Just ((), "")
                   _  -> Nothing)

  char c = PParse (\s -> case s of
                      c':s' | c == c' -> Just (c', s')
                      _               -> Nothing)

newtype Digit = Digit Char
  deriving (Show, Eq, Ord)

getDigit (Digit c) = c

instance Arbitrary Digit where
  arbitrary = oneof [return (Digit d) | d <- ['0'..'9']]

  shrink (Digit d) = [ Digit z | z <- shrink d, isDigit z ]

newtype Digits = Digits String
  deriving (Show, Eq, Ord)

getDigits (Digits ds) = ds

instance Arbitrary Digits where
  arbitrary = do
    ds <- arbitrary
    return (Digits $ map getDigit ds)

  shrink (Digits ds) = map Digits $
    shrinkList (\d -> [ d' | Digit d' <- shrink (Digit d) ]) ds

newtype Letter = Letter Char
  deriving (Show, Eq, Ord)

getLetter (Letter c) = c

instance Arbitrary Letter where
  arbitrary = oneof [ return (Letter c) | c <- ['a'..'z'] ++ ['A'..'Z'] ]

  shrink (Letter c) = [ Letter a | a <- shrink c, isAlpha a ]

newtype Letters = Letters String
  deriving (Show, Eq, Ord)

getLetters (Letters cs) = cs

instance Arbitrary Letters where
  arbitrary = do
    cs <- arbitrary
    return (Letters $ map getLetter cs)

  shrink (Letters cs) = map Letters $
    shrinkList (\c -> [ c' | Letter c' <- shrink (Letter c) ]) cs

newtype Spaces = Spaces String
  deriving (Show, Eq, Ord)

getSpaces (Spaces s) = s

instance Arbitrary Spaces where
  arbitrary = do
    n <- chooseInt (0, 255)
    return (Spaces $ replicate n ' ')

  shrink (Spaces s) = map Spaces $ shrinkList (:[]) s


infixr 8 :^:
infixl 7 :*:
infixl 7 :/:
infixl 6 :+:
infixl 6 :-:

data BaseExpr n a = Paren  a | Num n
  deriving (Eq, Functor)
data PowExpr    a = Base a   | a :^: PowExpr a
  deriving (Eq, Functor)
data MulExpr    a = Factor a | MulExpr a :*: a | MulExpr a :/: a
  deriving (Eq, Functor)
data AddExpr    a = Term   a | AddExpr a :+: a | AddExpr a :-: a
  deriving (Eq, Functor)

instance (Show n, Show a) => Show (BaseExpr n a) where
  show (Paren x) = "(" ++ show x ++ ")"
  show (Num   n) = show n

instance Show a => Show (PowExpr a) where
  show (Base b)  = show b
  show (b :^: e) = show b ++ " ^ " ++ show e

instance Show a => Show (MulExpr a) where
  show (Factor e) = show e
  show (m :*: e)  = show m ++ " * " ++ show e
  show (m :/: e)  = show m ++ " / " ++ show e

instance Show a => Show (AddExpr a) where
  show (Term m)  = show m
  show (s :+: m) = show s ++ " + " ++ show m
  show (s :-: m) = show s ++ " - " ++ show m


class Eval a where
  eval :: a -> Double

instance Eval Double where
  eval n = n

instance Eval Int where
  eval = fromIntegral

instance (Eval n, Eval a) => Eval (BaseExpr n a) where
  eval (Paren x) = eval x
  eval (Num n)   = eval n

instance Eval a => Eval (PowExpr a) where
  eval (Base b)  = eval b
  eval (b :^: e) = eval b ** eval e

instance Eval a => Eval (MulExpr a) where
  eval (Factor e) = eval e
  eval (m :*: e)  = eval m * eval e
  eval (m :/: e)  = eval m / eval e

instance Eval a => Eval (AddExpr a) where
  eval (Term m)  = eval m
  eval (m :+: s) = eval m + eval s
  eval (m :-: s) = eval m - eval s


instance (Num n, Arbitrary a) => Arbitrary (BaseExpr n a) where
  arbitrary = frequency [ (2, do n <- chooseInt (-1000, 1000)
                                 return (Num $ fromIntegral n)),
                          (1, Paren <$> arbitrary) ]

instance Arbitrary a => Arbitrary (PowExpr a) where
  arbitrary = frequency [ (2, Base <$> arbitrary),
                          (1, (:^:) <$> arbitrary <*> arbitrary) ]

instance Arbitrary a => Arbitrary (MulExpr a) where
  arbitrary = frequency [ (2, Factor <$> arbitrary),
                          (1, (:*:) <$> arbitrary <*> arbitrary),
                          (1, (:/:) <$> arbitrary <*> arbitrary) ]

instance Arbitrary a => Arbitrary (AddExpr a) where
  arbitrary = frequency [ (2, Term <$> arbitrary),
                          (1, (:+:) <$> arbitrary <*> arbitrary),
                          (1, (:-:) <$> arbitrary <*> arbitrary) ]

type RegularExpr' a = AddExpr (MulExpr (BaseExpr Int a))
data RegularExpr    = RExp (RegularExpr' RegularExpr)
  deriving (Eq)

type BonusExpr' a = AddExpr (MulExpr (PowExpr (BaseExpr Int a)))
data BonusExpr    = BExp (BonusExpr' BonusExpr)
  deriving (Eq)

instance Show RegularExpr where
  show (RExp s) = show s

instance Show BonusExpr where
  show (BExp s) = show s

instance Eval RegularExpr where
  eval (RExp s) = eval s

instance Eval BonusExpr where
  eval (BExp s) = eval s

instance Arbitrary RegularExpr where
  arbitrary = oneof [ RExp <$> Term <$> Factor <$> Num <$> arbitrary,
                      RExp <$> Term <$> Factor <$> arbitrary,
                      RExp <$> Term <$> arbitrary,
                      RExp <$> arbitrary ]

instance Arbitrary BonusExpr where
  arbitrary = oneof [ BExp <$> Term <$> Factor <$> Base <$> Num <$> arbitrary,
                      BExp <$> Term <$> Factor <$> Base <$> arbitrary,
                      BExp <$> Term <$> Factor <$> arbitrary,
                      BExp <$> Term <$> arbitrary,
                      BExp <$> arbitrary ]


-- alphabet = ['a'..'z']
alphabet = "abc"

newtype AlphaParseTable a = AlphaParse (ParseTable a)
  deriving (Show)

getAlphaParse (AlphaParse p) = p

instance Arbitrary a => Arbitrary (AlphaParseTable a) where
  arbitrary = frequency
    [ (4, AlphaParse . Done <$> arbitrary),
      (1, AlphaParse . Fork . take 4 . fmap getAlphaParse <$> arbitrary),
      (2, AlphaParse . Look . getAlphaTable <$> arbitrary) ]

newtype AlphaTable a = AlphaTable (M.Map (Maybe Char) (ParseTable a))
  deriving (Show)

getAlphaTable (AlphaTable t) = t

instance Arbitrary a => Arbitrary (AlphaTable a) where
  arbitrary =
    AlphaTable . (`M.restrictKeys` alphaCharSet) . fmap getAlphaParse <$> arbitrary
    where alphaCharSet = S.fromList (Nothing : fmap Just alphabet)

newtype AlphaChar = Alpha Char
  deriving (Show, Eq, Ord)

getAlpha (Alpha c) = c

instance Arbitrary AlphaChar where
  arbitrary = oneof [return (Alpha c) | c <- alphabet]

newtype AlphaString = Alphas String
  deriving (Show, Eq, Ord)

getAlphas (Alphas s) = s

instance Arbitrary AlphaString where
  arbitrary = Alphas . fmap getAlpha <$> arbitrary


arithAlphabet = [' ', '+', '-', '*', '/']
                ++
                ['0'..'9']

newtype ArithParseTable a = ArithParse (ParseTable a)
  deriving (Show)

getArithParse (ArithParse p) = p

instance Arbitrary a => Arbitrary (ArithParseTable a) where
  arbitrary = frequency
    [ (4, ArithParse . Done <$> arbitrary),
      (1, ArithParse . Fork . take 4 . fmap getArithParse <$> arbitrary),
      (2, ArithParse . Look . getArithTable <$> arbitrary) ]

newtype ArithTable a = ArithTable (M.Map (Maybe Char) (ParseTable a))
  deriving (Show)

getArithTable (ArithTable t) = t

instance Arbitrary a => Arbitrary (ArithTable a) where
  arbitrary =
    ArithTable . (`M.restrictKeys` arithCharSet) . fmap getArithParse <$> arbitrary
    where arithCharSet = S.fromList (Nothing : fmap Just arithAlphabet)

newtype ArithChar = Arith Char
  deriving (Show, Eq, Ord)

getArith (Arith c) = c

instance Arbitrary ArithChar where
  arbitrary = oneof [return (Arith c) | c <- arithAlphabet]

newtype ArithString = Ariths String
  deriving (Show, Eq, Ord)

getAriths (Ariths s) = s

instance Arbitrary ArithString where
  arbitrary = Ariths . fmap getArith <$> arbitrary

abc :: ParseTable String
abc = G.string "aba" <|> G.string "abb" <|> G.string "abc"

abcUnoptimized =
  Fork
  [ Fork
    [ Look (M.fromList
             [ (Just 'a',
                Look (M.fromList
                      [ (Just 'b',
                          Look (M.fromList [(Just 'a', Done "aba")]))]))]),
      Look (M.fromList
             [ (Just 'a',
                 Look (M.fromList
                       [ (Just 'b',
                          Look (M.fromList
                                [ (Just 'b', Done "abb")]))]))])],
    Look (M.fromList
           [ (Just 'a',
              Look (M.fromList
                    [ (Just 'b',
                        Look (M.fromList [(Just 'c',Done "abc")]))]))])]

abcOptimized =
  Look (M.fromList
         [ (Just 'a',
            Look (M.fromList
                  [ (Just 'b',
                      Look (M.fromList
                            [ (Just 'a', Done "aba"),
                              (Just 'b', Done "abb"),
                              (Just 'c', Done "abc")]))]))])

deriving instance Eq a => Eq (ParseTable a)

noFork1 (Done _)   = True
noFork1 (Look t)   = and [ noFork1 p | (_, p) <- M.toList t ]
noFork1 (Fork [_]) = False
noFork1 (Fork ps)  = and [ noFork1 p | p <- ps ]

noForkFork (Done _)  = True
noForkFork (Look t)  = and [ noForkFork p | (_, p) <- M.toList t ]
noForkFork (Fork ps) = and [ not (isFork p) && noForkFork p | p <- ps ]

isFork (Fork _) = True
isFork _        = False

noForkedLook (Done _)  = True
noForkedLook (Look t)  = and [ noForkedLook p | (_, p) <- M.toList t ]
noForkedLook (Fork ps) = length [ t | Look t <- ps ] <= 1
                      && and [ noForkedLook p | p <- ps ]

isOptimized (Done _)  = True
isOptimized (Look t)  = and [ isOptimized p | (_, p) <- M.toList t ]
isOptimized (Fork ps) = length [ t | Look t <- ps ] <= 1
                     && and [ not (isFork p) && isOptimized p | p <- ps ]

pruneParseTable d 0 _         = Done d
pruneParseTable d n (Done x)  = Done x
pruneParseTable d n (Look t)  = Look (pruneParseTable d (n-1) <$> t)
pruneParseTable d n (Fork ps) = Fork (pruneParseTable d (n-1) <$> ps)
