{-# LANGUAGE BlockArguments #-}
module Assignment where

import Grade

assignment :: Grader ()
assignment = do
  section "Section 1: Parser Combinators" do
    exercise "Regular Exercise 1.1" $ check 10 do
      part "NDParser.end" do
        must "successfully returns () when parsing the empty string"
        must "successfully leaves the empty string when parsing the empty string"
        must "fails whenever given a non-empty string"
        must "is deterministic (always fails or returns 1 success)"

    bonus $ exercise "Bonus Exercise 1.2" $ check 5 do
      part "NDParser.feed" do
        must "leaves an extra character in front of the remaining string to parse"
        must "'feed' followed by 'next' returns the fed character"
        must "'feed' followed by 'next' leaves the input string as it was"
        must "'next' followed by 'feed' does nothing on non-empty input"
        must "'next' followed by 'feed' fails when the input is empty"
        must "always succeeds no matter the input string"
        must "is deterministic (always fails or returns 1 success)"

    exercise "Regular Exercise 1.3" do
      part "NDParser.char" $ check 10 do
        must "fails on the empty input"
        must "fails when the next input character is different from expected"
        must "succeeds and returns the given character when it is the next input"
        must "removes the first character from the input when it succeeds"
        must "is deterministic (always fails or returns 1 success)"

    exercise "Regular Exercise 1.4" do
      part "NDParser.digit" $ check 5 do
        must "returns the next character of the input if it is '0' to '9'"
        must "removes the next digit from the input string, leaving the rest"
        must "fails if the next character in the input is not '0' to '9'"
        must "fails on the empty input string"
        must "is deterministic (always fails or returns 1 success)"

  section "Section 2: Arithmetic Expressions" do
    exercise "Regular Exercise 2.1" do
      part "Calculate.naturalNumber" $ check 4 do
        must "fails on the empty input"
        must "is the numeric value of 'digit' for just a single digit character"
        must "is the numeric value of 'digit' for a digit followed by a non-digit"
        must "can fully parse any sequence of digits as a natural number"
        must "correctly parses the shown representation of a non-negative integer"
        must "stops once it reaches a non-digit character"
      part "Calculate.negativeNumber" $ check 4 do
        must "fails on the empty input"
        must "fails on any input that doesn't begin with '-'"
        must "fails on an input beginning with '-' followed by a non-digit"
        must "negates the value returned by running negativeNumber after a '-'"
        must "fully parses the shown representation of any negative integer"
      part "Calculate.integer" $ check 2 do
        must "fails on the empty input"
        must "fails on the any input that doesn't begin with '-' or a digit"
        must "fails on any input that begins with '-' followed by a non-digit"
        must "fully parses the shown representation of *any* integer"
        must "stops once it reaches a non-digit character (maybe after '-')"

    bonus $ exercise "Bonus Exercise 2.2" do
      part "Calculate.decimalFraction" $ check 4 do
        must "fails on the empty input"
        must "is 'digit / 10' for just a single digit character"
        must "is 'digit / 10' for a digit followed by anything else"
        must "can fully parse any n-digit sequence as a fraction (over 10^n)"
        must "stops once it reaches a non-digit character"
      part "Calculate.float" $ check 4 do
        must "is the same as decimalFraction for input starting with '0.'"
        must "is the negation of decimalFraction for input starting with '-0.'"
        must "is the same as naturalNumber for '.0' following some digits"
        must "is the same as integer for '.0' following '-' and some digits"
        must "fully parses any floating point number (up to +/-10^6)"
      part "Calculate.number" $ check 2 do
        must "is the same as integer for input starting with a shown Int"
        must "is the same as 'float' for input starting with a shown Float"

    exercise "Regular Exercise 2.3" do
      part "Calculate.times" $ check 4 do
        must "succeeds when parsing '*'"
        must "fails when parsing anything else"
        must "is deterministic (always fails or returns 1 success)"
        must "leaves everything following '*'"
        must "returns the Haskell function matching '*'"
      part "Calculate.divide" $ check 4 do
        must "succeeds when parsing '/'"
        must "fails when parsing anything else"
        must "is deterministic (always fails or returns 1 success)"
        must "leaves everything following '/'"
        must "returns the Haskell function matching '/'"
      part "Calculate.multiplicative" $ check 2 do
        must "succeeds when parsing '*'"
        must "fails when parsing anything else"
        must "is deterministic (always fails or returns 1 success)"
        must "leaves everything following '*'"
        must "returns the Haskell function matching '*'"
        
        must "succeeds when parsing '/'"
        must "fails when parsing anything else"
        must "is deterministic (always fails or returns 1 success)"
        must "leaves everything following '/'"
        must "returns the Haskell function matching '/'"

    bonus $ exercise "Bonus Exercise 2.4" $ check 5 do
      part "Calculate.power" do
        must "succeeds when parsing '^'"
        must "fails when parsing anything else"
        must "is deterministic (always fails or returns 1 success)"
        must "leaves everything following '^'"
        must "returns the Haskell function matching '^'"

    exercise "Regular Exercise 2.5" $ split 10 do
      part "Calculate.trim" do
        must "is the same as the given parser when there are no spaces"
        must "skips blank spaces before the real text to parse"
        must "skips blank spaces after the real text to parse"
        must "skips blank spaces before *and* after the real text to parse"

    exercise "Regular Exercise 2.6" $ split 10 do
      part "Calculate.applyBinR" do
        must "passes a value as the 'right' (i.e., 2nd) argument to the function"
      part "Calculate.compose" do
        must "calls the 1st function, then passes its result to the 2nd function"
      part "Calculate.applyL" do
        must "passes the value to the function and returns its result"

    exercise "Regular Exercise 2.7" do
      part "Calculate.baseExpr" $ split 3 do
        must "can parse integers with or without parentheses"
        must "parses EITHER an additive expression in parentheses or an integer"
      part "Calculate.addExpr" $ split 3 do
        must "can parse sums (+ or -) of integers"
        must "parses a left-associated summation of multiplicative expressions"
        must "parses a left-associated summation of nested expressions"
      part "Calculate.mulExpr" $ split 3 do
        must "can parse products (* or /) of integers"
        must "parses a left-associated product of base expressions"
        must "parses a left-associated product of nested expressions"
      part "Calculate.expr" $ check 1 do
        must "uniquely parses arbitrarily nested expressions"

    bonus $ exercise "Bonus Exercise 2.8" do
      part "Calculate.powExpr" $ split 10 do
        must "can parse exponential towers of integers"
        must "can parse right-assicated exponents of expressions"
      part "Calculate.expr" $ check 5 do
        must "uniquely parses arbitrarily nested expressions (with exponents)"

  bonus $ section "Bonus Section 3: Parser API" do
    exercise "Bonus Exercise 3.1" do
      part "Parser NDParser => GenericParser.end" $ check (5/2) do
        must "successfully returns () when parsing the empty string"
        must "successfully leaves the empty string when parsing the empty string"
        must "fails whenever given a non-empty string"
        must "is deterministic (always fails or returns 1 success)"
        must "is exactly the same as NDParser.end for every input"
      part "Parser NDParser => GenericParser.char" $ check (5/2) do
        must "fails when parsing the empty string"
        must "succeeds when the input starts with the given character"
        must "fails when the input starts with the any other character"
        must "always leaves everything after the first character when it succeeds"
        must "is deterministic (always fails or returns 1 success)"
        must "is exactly the same as NDParser.char for every input"

    exercise "Bonus Exercise 3.2" $ check 10 do
      part "GenericParser.string" do
        must "succeeds when parsing the empty string in the empty input"
        must "fails when parsing a non-empty string in the empty input"
        must "succeeds when the input starts with *exactly* the given string"
        must "fails when the input starts with the any other string"
        must "fails even with the input starts with a portion of the string"
        must "always leaves everything after the given string when it succeeds"
        must "is deterministic (always fails or returns 1 success)"

    exercise "Bonus Exercise 3.3" do
      part "GenericParser.oneOf" $ check 2 do
        must "works for any instance of Parser"
        must "returns the next character of the input is one of the given ones"
        must "on success, removes the character from the input, leaving the rest"
        must "fails if the next input character is not one of the given ones"
        must "fails on the empty input string"
        must "is deterministic (when given distinct choices)"
      part "GenericParser.space" $ check 2 do
        must "works for any instance of Parser"
        must "returns the next character of the input if it is a space"
        must "removes the next space from the input string, leaving the rest"
        must "fails if the next character in the input is not a space"
        must "fails on the empty input string"
        must "is deterministic (always fails or returns 1 success)"
      part "GenericParser.spaces" $ check 2 do
        must "works for any instance of Parser"
        must "succeeds on the empty input"
        must "succeeds when the input is just a single space character"
        must "succeeds when the input is a single space followed by anything else"
        must "can fully parse any sequence of spaces"
        must "stops once it reaches a non-space character"
      part "GenericParser.digit" $ check 2 do
        must "works for any instance of Parser"
        must "returns the next character of the input if it is '0' to '9'"
        must "removes the next digit from the input string, leaving the rest"
        must "fails if the next character in the input is not '0' to '9'"
        must "fails on the empty input string"
        must "is deterministic (always fails or returns 1 success)"
      part "GenericParser.digits" $ check 2 do
        must "works for any instance of Parser"
        must "fails on the empty input"
        must "succeeds when the input is just a single digit character"
        must "succeeds when the input is a single digit followed by anything else"
        must "can fully parse any sequence of digits"
        must "stops once it reaches a non-digit character"

    exercise "Bonus Exercise 3.4" $ split 20 do
      part "Calculate" do
        must "naturalNumber has been generalized to use the 'Parser m' interface"
        must "negativeNumber has been generalized to use the 'Parser m' interface"
        must "integer has been generalized to use the 'Parser m' interface"
        must "plus has been generalized to use the 'Parser m' interface"
        must "minus has been generalized to use the 'Parser m' interface"
        must "additive has been generalized to use the 'Parser m' interface"
        must "times has been generalized to use the 'Parser m' interface"
        must "divide has been generalized to use the 'Parser m' interface"
        must "multiplicative has been generalized to use the 'Parser m' interface"
        must "trim has been generalized to use the 'Parser m' interface"
        must "parenthesized has been generalized to use the 'Parser m' interface"
        must "binopR has been generalized to use the 'Parser m' interface"
        must "chainR has been generalized to use the 'Parser m' interface"
        must "assocL has been generalized to use the 'Parser m' interface"
        must "baseExpr has been generalized to use the 'Parser m' interface"
        must "mulExpr has been generalized to use the 'Parser m' interface"
        must "addExpr has been generalized to use the 'Parser m' interface"
        must "decimalFraction has been generalized to use the 'Parser m' interface"
        must "float has been generalized to use the 'Parser m' interface"
        must "number has been generalized to use the 'Parser m' interface"
        must "power has been generalized to use the 'Parser m' interface"
        must "binop has been generalized to use the 'Parser m' interface"
        must "assocR has been generalized to use the 'Parser m' interface"
        must "powExpr has been generalized to use the 'Parser m' interface"

  bonus $ section "Bonus Section 4: Parse Tables" do
    exercise "Bonus Exercise 4.1" do
      part "ParseTable (string \"aba\" <|> string \"abb\" <|> string \"abc\")" $ check 1 do
        must "parses the string \"aba\" followed by anything"
        must "parses the string \"abb\" followed by anything"
        must "parses the string \"abc\" followed by anything"
        must "fails for any other input"
      part "ParseTable.parseTable (Done x)" $ check 3 do
        must "always succeeds on any input string"
        must "always returns x regardless of input string"
        must "leaves the input string alone"
      part "ParseTable.parseTable (Fork ps)" $ split 3 do
        must "combines the possible returned values from a list of Done parsers"
        must "always leaves the input string alone when each parser in `ps` does"
        must "always fails when `ps` is empty"
        must "is the same as `p` when `ps = [p]`"
        must "combines the results of `p1` and `p2` when `ps = [p1, p2]`"
        must "fails on any input where every parser in `ps` fails"
        must "succeeds on any input where just one parser in `ps` succeeds"
        must "combines all returned results from multiple successful parsers"
      part "ParseTable.parseTable (Look table)" $ split 3 do
        must "is deterministic when all the parsers in `table` are"
        must "always fails when the `table` is empty"
        must "succeeds on the empty input if `table` has a `Nothing` value"
        must "reads the first character of the input if it is Just in the table"
        must "fails when the first character of the input is not in the table"
        must "is the same as the parser associated with the first input character"

    exercise "Bonus Exercise 4.2" do
      part "Parser ParseTable => GenericParser.end" $ check 5 do
        must "successfully returns () when parsing the empty string"
        must "successfully leaves the empty string when parsing the empty string"
        must "fails whenever given a non-empty string"
        must "is deterministic (always fails or returns 1 success)"
        must "is the same as the NDParser instance"
      part "Parser ParseTable => GenericParser.char" $ check 5 do
        must "fails when parsing the empty string"
        must "succeeds when the input starts with the given character"
        must "fails when the input starts with the any other character"
        must "always leaves everything after the first character when it succeeds"
        must "is deterministic (always fails or returns 1 success)"
        must "is exactly the same as NDParser instance"

    exercise "Bonus Exercise 4.3" do
      part "ParseTable.immediate" $ split 10 do
        must "returns exactly one immediate result [x] for Done x"
        must "is always empty [] for a Look"
        must "combines all the immediate results of the parsers in a Fork"
      part "ParseTable.lookahead" $ split 10 do
        must "returns an empty table for a Done parser"
        must "returns the table inside Look, as-is"
        must "returns the union of all lookahead results from a Fork"

    exercise "Bonus Exercise 4.4" do
      part "ParseTable.inlineFork1" $ check 5 do
        must "can simplify just one `Fork [p]` to `p` at the top of the parser"
        must "leaves any other form of parser alone"
      part "ParseTable.optimize" $ split 20 do
        must "correctly optimizes string \"aba\" <|> string \"abb\" <|> string \"abc\""
        must "never gives a parser with a Fork of one option"
        must "never gives a parser with a Fork of a Fork"
        must "never gives a parser with a Fork containing two Look tables"
        must "is idempotent (optimizing twice is the same as optimizing once)"
        must "correctly preserves the behavior of parser failures"
        must "correctly preserves the behavior of successful parsing"

    exercise "Bonus Exercise 4.5" do
      part "Calculate.fastExpr" $ check 5 do
        must "is optimized"
        must "uniquely parses arbitrarily nested expressions"
        -- must "uniquely parses arbitrarily nested expressions (with exponents)"
