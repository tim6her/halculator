import System.IO

type Stack = [Float]

main = calc []

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
                        parsing (Just stack) $ 
                        words comm
        putStr "Out: "
        putStrLn $ showHead stack'
        calc stack'

showHead :: Stack -> String
showHead [] = "Nothing"
showHead (x:stack) = show x

updateStack :: Stack -> Maybe Stack -> IO Stack
updateStack stack Nothing = do 
    putStrLn "Can't perform operation"
    return stack
updateStack _ (Just stack') = return stack'

parsing :: Maybe Stack -> [String] -> Maybe Stack
parsing = foldl (\mstack com -> mstack >>= flip pars com)

pars :: Stack -> String -> Maybe Stack
-- Operators
pars (x:y:stack) "+" = Just $ (y + x) : stack
pars (x:y:stack) "-" = Just $ (y - x) : stack
pars (x:y:stack) "*" = Just $ (y * x) : stack
pars (x:y:stack) "/" = Just $ (y / x) : stack
pars (x:y:stack) "^" = Just $ (y ** x) : stack
-- Misc
pars stack "sum" = Just $ (sum stack) : []
pars stack "prod" = Just $ (foldl (*) 1 stack) : []
pars (x:stack) "!" = Just $ (fak x) : stack
pars (x:stack) "abs" = Just $ (abs x) : stack
-- Constants
pars stack "pi" = Just $ pi : stack
pars stack "e" = Just $ (exp 1.0) : stack
-- Functions with Exponents
pars (x:stack) "exp" = Just $ (exp x) : stack
pars (x:stack) "log" = Just $ (log x) : stack
pars stack "ln" = pars stack "log"
pars (x:stack) "sqrt" = Just $ (sqrt x) : stack
pars (x:y:stack) "logBase" = Just $ (logBase y x) : stack
-- Trigonomitry and Hyperbolics
pars (x:stack) "sin" = Just $ (sin x) : stack
pars (x:stack) "cos" = Just $ (cos x) : stack
pars (x:stack) "tan" = Just $ (tan x) : stack
pars (x:stack) "asin" = Just $ (asin x) : stack
pars (x:stack) "acos" = Just $ (acos x) : stack
pars (x:stack) "atan" = Just $ (atan x) : stack
pars (x:stack) "sinh" = Just $ (sinh x) : stack
pars (x:stack) "cosh" = Just $ (cosh x) : stack
pars (x:stack) "tanh" = Just $ (tanh x) : stack
pars (x:stack) "asinh" = Just $ (asinh x) : stack
pars (x:stack) "acosh" = Just $ (acosh x) : stack
pars (x:stack) "atanh" = Just $ (atanh x) : stack
-- Control
pars (x:y:stack) "swp" = Just $ y : x : stack
pars stack@(x:xs) "rot" = Just $ (last stack) : (init stack)
pars stack "rotl" = pars stack "rot"
pars (x:stack) "rotr" = Just $ stack ++ [x]
pars _ "clr" = Just []
pars stack comm
    | contNum comm = Just $ (read comm) : stack
    | otherwise = Nothing

contNum :: String -> Bool
contNum comm = any (\ n -> n `elem` comm) ['0'..'9']

fak :: Float -> Float
fak x
    | x' == 0.0 = 1.0
    | x' == 1.0 = 1.0
    | x < 0 = error "Negativ factorial"
    | otherwise = x' * (fak $ x' - 1.0)
    where x' = fromIntegral (truncate x :: Int)
