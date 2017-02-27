# halculator
A simple RPN calculator written in Haskell
---
![status](https://travis-ci.org/tim6her/halculator.svg?branch=master)

Ever wanted to find the expected number of heads when 
tossing 5 coins, i. e., computing

![expected value](imgs/binom.png),

using an [RPN](https://en.wikipedia.org/wiki/Reverse_Polish_notation) calculator? Now you can!

![screen2](imgs/screen2.gif)

### But wait there's more

Currently the following functions are implemented:

* Operators
  * `+`
  * `-`
  * `*`
  * `/`
  * `^`
* Misc
  * `sum` (sum of all elements in stack)
  * `prod` (product of all elements in stack)
  * `!` (factorial)
  * `nCr` (n choose r)
  * `abs`
  * `..` (range between two values)
* Constants
  * `pi`
  * `e`
* Functions with Exponents
  * `exp`
  * `log` (logarithm with respect to Euler constant `e`)
  * `ln` (alias for `log`)
  * `sqrt`
  * `logBase` (Logarithm of `x` with respect to base `y`)
* Trigonomitry and Hyperbolics
  * `sin`
  * `cos`
  * `tan`
  * `asin`
  * `acos`
  * `atan`
  * `sinh`
  * `cosh`
  * `tanh`
  * `asinh`
  * `acosh`
  * `atanh`
* Control
  * `swp` (swap the first two elements in stack)
  * `cpy` (copy first element)
  * `rot` (rotate stack one element to the left)
  * `rotl` (alias for `rot`)
  * `rotr` (rotate stack one element to the right)
  * `clr` (clear stack)

For a documentation of the implementation please visit https://tim6her.github.io/halculator/index.html