{-|
Module      : Main
Description : The backbone of the executable
Copyright   : (c) Tim B. Herbstrith, 2017
License     : MIT
Maintainer  : Please contact me via GitHub!
Stability   : experimental
Portability : POSIX

A simple RPN calculator written in Haskell
-}
module Main where

import System.IO

-- * Functions for user interactions

-- | All numerals for computations are stored in
-- a stack of arbitrary size
type Stack = [Float]

-- | Starts the main recursion with empty stack
main = calc []

-- | Gets command and updates stack according
-- to user input.
--
-- The command "show" will print the stack.  All other
-- inputs will be passed to parsing.  At the end 'calc'
-- is called with the updated stack.
calc :: Stack -> IO ()
calc stack = do
    putStr "In: "
    hFlush stdout
    comm <- getLine
    if comm == "show"
    then do
        putStrLn $ show (reverse stack) ++ " <-"
        calc stack 
    else do
        stack' <- updateStack stack $
                        parsing stack $
                        words comm
        putStr "Out: "
        putStrLn $ showHead stack'
        calc stack'

-- | Returns string representation of the first item
-- on the stack.  If the stack is empty, the string
-- "Nothing" is returned.
showHead :: Stack -> String
showHead [] = "Nothing"
showHead (x:stack) = show x

-- | Safely updates the stack by checking first if
-- 'Maybe' 'Stack' is 'Nothing' the user will be
-- informed and the old stack is returned.
-- Otherwise the stack is extracted and returned.
updateStack :: Stack        -- ^ The old stack
            -> Maybe Stack  -- ^ Hopefully the new one
            -> IO Stack
updateStack stack Nothing = do 
    putStrLn "Can't perform operation"
    return stack
updateStack _ (Just stack') = return stack'

-- * Functions for parsing

-- | Takes a list of commands and applys them
-- sequencially to the stack.
--
-- If a computation fails, 'Nothing' is returned.
--
-- === Examples:
-- >>> parsing [] $ words "3 2 ^ 4 2 ^ + sqrt"
-- Just [5.0]
-- >>> parsing [2.0] ["/"] -- Too few arguments on stack!
-- Nothing
parsing :: Stack -> [String] -> Maybe Stack
parsing stack = foldl pars' mstack
    where mstack = Just stack
          pars' mstack' com = do
                      stack' <- mstack'
                      pars stack' com

-- | Parses command and applys the corresponding
-- function to the first elements of the stack.
--
-- If the command is not recognised or there are
-- too few numbers on the stack, 'Nothing' is
-- returned.
--
-- === Examples:
-- >>> let stack = [-6.0, 2.0] :: Stack
-- >>> pars stack "*"
-- Just [-12.0]
-- >>> pars stack "abs"
-- Just [6.0,2.0]
-- >>> pars stack ".."
-- Just [-6.0,-5.0,-4.0,-3.0,-2.0,-1.0,0.0,1.0,2.0]
-- >>> pars stack "katze" -- I can't imagine such a command exists
-- Nothing
pars :: Stack -> String -> Maybe Stack
-- Operators
pars (x:y:stack) "+" = Just $ (y + x) : stack
pars (x:y:stack) "-" = Just $ (y - x) : stack
pars (x:y:stack) "*" = Just $ (y * x) : stack
pars (x:y:stack) "/" = Just $ (y / x) : stack
pars (x:y:stack) "^" = Just $ (y ** x) : stack
-- Misc
pars stack "sum" = Just [sum stack]
pars stack "prod" = Just [product stack]
pars (x:stack) "!" = Just $ fac x : stack
pars (x:y:stack) "nCr" = Just $ nCr y x : stack
pars (x:stack) "abs" = Just $ abs x : stack
pars (x:y:stack) ".." = let a = truncate y
                            b = truncate x
                            a' = a + signum (b - a)
                        in Just $ map fromIntegral 
                                          (reverse [a,a'..b])
                                  ++ stack
-- Constants
pars stack "pi" = Just $ pi : stack
pars stack "e" = Just $ exp 1.0 : stack
-- Functions with Exponents
pars (x:stack) "exp" = Just $ exp x : stack
pars (x:stack) "log" = Just $ log x : stack
pars stack "ln" = pars stack "log"
pars (x:stack) "sqrt" = Just $ sqrt x : stack
pars (x:y:stack) "logBase" = Just $ logBase y x : stack
-- Trigonomitry and Hyperbolics
pars (x:stack) "sin" = Just $ sin x : stack
pars (x:stack) "cos" = Just $ cos x : stack
pars (x:stack) "tan" = Just $ tan x : stack
pars (x:stack) "asin" = Just $ asin x : stack
pars (x:stack) "acos" = Just $ acos x : stack
pars (x:stack) "atan" = Just $ atan x : stack
pars (x:stack) "sinh" = Just $ sinh x : stack
pars (x:stack) "cosh" = Just $ cosh x : stack
pars (x:stack) "tanh" = Just $ tanh x : stack
pars (x:stack) "asinh" = Just $ asinh x : stack
pars (x:stack) "acosh" = Just $ acosh x : stack
pars (x:stack) "atanh" = Just $ atanh x : stack
-- Control
pars (x:y:stack) "swp" = Just $ y : x : stack
pars stack@(x:xs) "cpy" = Just $ x : stack
pars stack@(x:xs) "rot" = Just $ last stack : init stack
pars stack "rotl" = pars stack "rot"
pars (x:stack) "rotr" = Just $ stack ++ [x]
pars _ "clr" = Just []
pars (x:xs) "del" = Just xs
pars stack comm
    | contNum comm = Just $ read comm : stack
    | otherwise = Nothing

-- * Operators and Helper Functions

-- | Checks whether a string contains a numeral
contNum :: String -> Bool
contNum comm = any (`elem` comm) ['0'..'9']

-- | A @Float@ implementation for the factorial
--
-- === Note:
-- Floats will be truncated.
--
-- === Example:
-- >>> fac 4.2
-- 24.0
fac :: (RealFrac a, Eq a, Ord a) => a -> a
fac x
    | x' == 0.0 = 1.0
    | x' == 1.0 = 1.0
    | x < 0 = 0.0 / 0.0 -- NaN representation in GHC
    | otherwise = x' * fac (x' - 1.0)
    where x' = fromIntegral (truncate x :: Int)

-- | A 'Float' implementation for "n choose r"
--
-- === Note:
-- Floats will be truncated.
nCr :: Float -> Float -> Float
nCr n r = fromIntegral $ nCr' (truncate n) (truncate r)

-- | The backbone of 'nCr'
nCr' :: (Integral a) => a -> a -> a
nCr' n r
    | n < r || r < 0 = div 0 0
    | r > div n 2 = nCr' n (n - r)
    | r == 0 = 1
    | r == 1 = n
    | otherwise = nCr' (n - 1) (r - 1) + nCr' (n - 1) r
