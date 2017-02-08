# halculator
A simple RPN calculator written in Haskell
---

![screen](https://cloud.githubusercontent.com/assets/11040405/22739387/bb2035a8-ee0b-11e6-918c-2bde440c2c97.gif)

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
  * `abs`
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
  * `swp` (swap the first to elements in stack)
  * `rot` (rotate stack one element to the left)
  * `rotl` (alias for `rot`)
  * `rotr` (rotate stack one element to the right)
  * `clr` (clear stack)