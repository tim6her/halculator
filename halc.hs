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
            stack' <- getStack stack $ 
                            parsing (Just stack) $ 
                            words comm
            putStr "Out: "
            putStrLn $ showHead stack'
            calc stack'

showHead :: Stack -> String
showHead [] = "Nothing"
showHead (x:stack) = show x

getStack :: Stack -> Maybe Stack -> IO Stack
getStack stack Nothing = do 
    putStrLn "Can't perform operation"
    return stack
getStack _ (Just stack') = return stack'

parsing :: Maybe Stack -> [String] -> Maybe Stack
parsing (Just stack) [] = Just stack
parsing Nothing _ = Nothing
parsing (Just stack) (c:cs) = parsing stack' cs
    where stack' = pars stack c

pars :: Stack -> String -> Maybe Stack
pars (x:y:stack) "+" = Just $ (y + x) : stack
pars (x:y:stack) "-" = Just $ (y - x) : stack
pars (x:y:stack) "*" = Just $ (y * x) : stack
pars (x:y:stack) "/" = Just $ (y / x) : stack
pars (x:y:stack) "^" = Just $ (y ** x) : stack
pars (x:stack) "!" = Just $ (fak x) : stack
pars (x:stack) "abs" = Just $ (abs x) : stack
pars stack "sum" = Just $ (sum stack) : []
pars (x:y:stack) "swp" = Just $ y : x : stack
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
