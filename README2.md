# Extended Functional Calculator

## Description
This project extends the arithmetic calculator from Assignment 4 into
a more complete interactive system. It supports variables, built-in 
functions, and user-defined functions, making it behave like a small
calculator language.

## Features
- Basic arithmetic expressions (+, -, *, /, ^)
- Variables:
    x = 5
    x + 2
- Built-in functions:
  - sqrt(x)
  - abs(x)
  - sin(x)
  - cos(x)
- User-defined functions:
    f(x) = x * 2
    f(5)
- Nested expressions:
    sqrt(f(8))

## How to Run
stack build
stack run


## Example Usage
> 1 + 2 * 3
7.0

> 2 ^ 3
8.0
 
> 10.2 - 4.1
6.1

> x = 5
Stored x = 5.0
> y = 10 
Stored y = 10.0
> (x + y) * 2   
30.0

> sqrt(9)
3.0

> abs(-5)
5.0

> sin(0)
0.0

> cos(0)
1.0

> pi
3.141592653589793

> sin(pi/2)
1.0

> x = 5
Stored x = 5.0
> sqrt(x * 4)
4.47213595499958

> f(x) = x * 2
Stored function f
> f(5)
10.0
> f(3 + 2)
10.0

> x = 7
Stored x = 7.0
> f(x)
14.0

> sqrt(f(8))
4.0
> sin(f(1))
0.9092974268256817

> z + 1
Error: variable 'z' is not defined.
> 1 +
Not an arithmetic expression: 1 +

## Notes
The implementation builds on parser combinators from Assignment 4 and
extends the interpreter to maintain state for variables and
functions.