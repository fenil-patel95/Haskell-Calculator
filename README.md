Assignment 4: Calculator
========================

Section 1 asks you to implement some basic parser functionality for NDParsers.
The template file src/NDParser.hs contains some definitions to get started, and
includes an outline (including a type signature and an "undefined"
function/value header) for each parser or function the required exercises in
Section 1 ask you to implement.  Replace every "undefined" in the template with
your answer to the corresponding exercise to complete Section 1.

Section 2 asks you to implement parsers for arithmetic expressions.  The
template file src/Calculate.hs contains some helper functions/parsers and an
outline for each required exercise in Section 2.  Replace every "undefined" in
the src/Calculate.hs template with your answer to the corresponding exercise to
complete Section 1.

Extra Credit
------------

Sections 1 and 2 include a few bonus exercises that you can answer by adding
your code to the template file corresponding to the section.

Sections 3 and 4 are purely optional, and give you the opportunity to collect a
significant amount of extra credit.

Section 3 asks you to implement a generic Parser API given in the template file
src/GenericParser.hs, which you can do by filling in this template file.
Section 3 also asks that you generalize your arithmetic parsers, which requires
you to update the definitions in src/Calculate.hs as described in the
assignment.

Section 4 asks you to work on a different implementation of parsing based on
lookup tables.  The template file src/ParseTable.hs provides an outline for
completing the bonus exercises of Section 4.  Finally, the last bonus exercise
in Section 4 asks you to update a few lines in src/Calculate.hs and the Main
application in app/Main.hs to use ParseTables.

Testing
-------

You can test your answers using the provided test suite found in the `test`
directory.  Passing all tests of mandatory exercises will ensure a %100 score
(or > %100 if some bonus exercises are passed as well).  Any failing test for an
exercise shows that there is some problem in your code.

This test suite can be run automatically using `cabal` with the command

    cabal test
	
or using `stack` with the command

    stack test
	
While the formatting of their output differs somewhat (for example, `stack` uses
colors to differentiate successful versus failing properties), their results
will be the same.

You can pass some additional test options to selectively run only certain tests.
If you want to only run the tests for "regular" exercises, required to finish
the assignment %100, you can use one of the two commands:

    cabal test --test-options="--match Regular"
	
or

    stack test --test-arguments "--match Regular"

Inversely, if you only want to run the tests for "bonus" exercises, which can
earn extra credit, use one of the two commands

    cabal test --test-options="--match Bonus"
	
or

    stack test --test-arguments "--match Bonus"

### CAUTION

For your own good, do not modify anything in the `test` directory!
