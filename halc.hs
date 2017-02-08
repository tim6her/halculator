import System.IO

data Stack a = EmptyStack | Push a (Stack a)
    deriving (Read, Eq, Ord) 

instance (Show a) => Show (Stack a) where
    show = (++ " <-") . show . reverse . toList 

pop :: Stack b -> (Maybe b, Stack b)
pop  EmptyStack = (Nothing, EmptyStack)
pop (Push x stack) = (Just x, stack)

popN :: (Integral a, Eq b) => a -> Stack b -> 
                              (Maybe [b], Stack b)
popN _ EmptyStack = (Nothing, EmptyStack)
popN 1 (Push x stack) = (Just [x], stack)
popN n (Push x stack)
    | n < 1 = error "Can't return less than one item"
    | otherwise = if may == Nothing
                  then (Nothing, EmptyStack)
                  else let (Just xs) = may
                       in (Just (x:xs), stack')
    where (may, stack') = popN (n - 1) stack

push :: b -> Stack b -> Stack b
push x stack = Push x stack

showFst :: (Show b) => Stack b -> String
showFst EmptyStack = "Nothing"
showFst (Push x _) = show x

toList :: Stack b -> [b]
toList EmptyStack = []
toList (Push x stack) = x:toList stack


main = calc EmptyStack

calc :: Stack Float -> IO ()
calc stack = do
    putStr "In: "
    hFlush stdout
    comm <- getLine
    if comm == "show"
        then do
            print stack
            calc stack 
        else do
            stack' <- getStack stack $ pars stack comm
            putStr "Out: "
            putStrLn $ showFst stack'
            calc stack'

getStack :: (Eq a) => Stack a -> Maybe (Stack a) -> 
                      IO (Stack a)
getStack stack Nothing = do 
    putStrLn "Can't perform operation"
    return stack
getStack _ (Just stack') = return stack'

pars :: Stack Float -> String -> Maybe (Stack Float)
pars (Push x (Push y stack)) "+" = Just $ push (y + x) stack
pars (Push x (Push y stack)) "-" = Just $ push (y - x) stack
pars (Push x (Push y stack)) "*" = Just $ push (y * x) stack
pars (Push x (Push y stack)) "/" = Just $ push (y / x) stack
pars (Push x (Push y stack)) "^" = Just $ push (y ** x) stack
pars (Push x stack) "!" = Just $ push (fak x) stack
pars (Push x stack) "abs" = Just $ push (abs x) stack
pars stack "sum" = Just $ push (sum $ toList stack) EmptyStack
pars (Push x (Push y stack)) "swp" = Just $ push y (push x stack)
pars _ "clr" = Just EmptyStack
pars stack comm
    | contNum comm = Just $ push (read comm :: Float) stack
    | otherwise = Nothing

contNum :: String -> Bool
contNum comm = any (\ n -> n `elem` comm) ['0'..'9']

fak :: Float -> Float
fak x
    | x' == 0.0 = 1.0
    | x' == 1.0 = 1.0
    | x < 0 = error "Negativ factorial"
    | otherwise = x' * (fak $ x' - 1.0)
    where x' = (fromIntegral (truncate x :: Int) :: Float)
