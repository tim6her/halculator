data Stack a = EmptyStack | Push a (Stack a)
    deriving (Read, Eq, Ord) 

instance (Show a) => Show (Stack a) where
    show = (++ " <-") . show . reverse . toList 

pop :: Stack b -> (b, Stack b)
pop  EmptyStack = error "Not enough elemenst in stack"
pop (Push x stack) = (x, stack)

push :: b -> Stack b -> Stack b
push x stack = Push x stack

showFst :: Stack b -> b
showFst EmptyStack = error "Not enough elemenst in stack"
showFst (Push x _) = x

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
            let stack' = pars stack comm
            putStr "Out: "
            print $ showFst stack'
            calc stack'

pars :: Stack Float -> String -> Stack Float
pars stack comm
    | comm == "+" = push (y + x) stack''
    | comm == "-" = push (y - x) stack''
    | comm == "*" = push (y * x) stack''
    | comm == "/" = push (y / x) stack''
    | comm == "abs" = push (abs x) stack'
    | comm == "swp" = push y $ push x stack''
    | otherwise = push (read comm :: Float) stack
    where (x, stack') = pop stack
          (y, stack'') = pop stack'
