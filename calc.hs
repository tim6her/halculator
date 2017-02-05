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
pars stack comm
    | comm == "+" = applyBinOp (+) stack
    | comm == "-" = applyBinOp (-) stack
    | comm == "*" = applyBinOp (*) stack
    | comm == "/" = applyBinOp (/) stack
    | comm == "^" = applyBinOp (**) stack
    | comm == "!" = applyUnOp fak stack
    | comm == "abs" = applyUnOp abs stack
    | comm == "sum" = Just $ push (sum $ toList stack) EmptyStack
    | comm == "swp" = swap stack
    | comm == "clr" = Just EmptyStack
    | otherwise = Just $ push (read comm :: Float) stack

applyUnOp :: (Float -> Float) -> 
               Stack Float -> Maybe (Stack Float)
applyUnOp op stack
    | fst (pop stack) == Nothing = Nothing
    | otherwise = Just $ push (op x) stack'
    where (Just x, stack') = pop stack

applyBinOp :: (Float -> Float -> Float) -> 
               Stack Float -> Maybe (Stack Float)
applyBinOp op stack
    | fst (popN 2 stack) == Nothing = Nothing
    | otherwise = Just $ push (op y x) stack'
    where (Just [x,y], stack') = popN 2 stack

swap :: Stack Float -> Maybe (Stack Float)
swap stack
    | fst (popN 2 stack) == Nothing = Nothing
    | otherwise = Just $ push y $ push x stack'
    where (Just [x,y], stack') = popN 2 stack

fak :: Float -> Float
fak x
    | x' == 0.0 = 1.0
    | x' == 1.0 = 1.0
    | x < 0 = error "Negativ factorial"
    | otherwise = x' * (fak $ x' - 1.0)
    where x' = (fromIntegral (truncate x :: Int) :: Float)
