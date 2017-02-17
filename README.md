# halculator
A simple RPN calculator written in Haskell
---
![status](https://travis-ci.org/tim6her/halculator.svg?branch=master)

Ever wanted to find the expected number of heads when 
tossing 5 coins, i. e., computing

![expected value](http://latex.univie.ac.at/?%5Cmathbb%20E%5Cleft%5B%5Cmathrm%7BBinom%7D%5Cleft%285,%20%5Cfrac%7B1%7D%7B2%7D%5Cright%29%5Cright%5D%20%3D%20%5Csum_%7Bi%3D0%7D%5E5%20i%20%7B5%20i%20%5Cchoose%20i%7D%20%5Cleft%28%5Cfrac%7B1%7D%7B2%7D%5Cright%29%5E5),

using an [RPN](https://en.wikipedia.org/wiki/Reverse_Polish_notation) calculator? Now you can!

![screen2](docs/screen2.gif)

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