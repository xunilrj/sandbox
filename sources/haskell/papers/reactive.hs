module Main where
    import Data.Function
    --Identity monad
    type I a = a
    --Identity Function
    unitI :: a -> a
    unitI a = a
    -- plain postfix application (always call)
    bindI :: a -> (a -> I b) -> I b
    bindI a f = f a
    -- just creating as a common f# operator
    -- https://stackoverflow.com/a/5758326/5397116
    (|>) :: a -> (a -> I b) -> I b
    (|>) a f = f a 
    --showI :: a -> String
    showI a = show a
    
    f = \x -> x*2       
    -- lift one to the I monad
    i1 = unitI 1
    -- bind I1 to f. In the I world, this is the same as applying f to i1
    i2 = i1 `bindI` f
    -- same as the two last lines in just one line
    i3 = (unitI 1) `bindI` f    
    -- same thing without monads involved with the fsharp operator
    i4 = 1 |> f
    -- same thing with the default haskell operator
    i5 = 1 & f

    --Monad M
    data M a = M a
    unitM :: a -> M a
    unitM a = M a
    bindM :: M a -> (a -> M b) -> M b
    bindM (M a) f = f a

    --Monad E
    data E a = Ok a | Fail String
    unitE :: a -> E a
    unitE a = Ok a
    failE :: String -> E a
    failE s = Fail s
    -- conditional postfix application (only when success)
    -- on error I do not call f, and propagates the error.
    bindE :: E a -> (a -> E b) -> E b
    bindE (Ok a) f = f a
    bindE (Fail s) f = Fail s
    showE :: (Show a) => E a -> String
    showE (Ok a) = "OK " ++ (show a)
    showE (Fail s) = "FAIL " ++ s

    --Monad S
    -- this looks like a shadow state that will run between the main
    -- functions.
    type S a b = b -> (a, b)
    --actually creates a pair with the iteration argument and the current state
    unitS :: a -> S a b
    unitS a = \s0 -> (a, s0)
    bindS :: S a b -> (a -> S a c) -> S a c
    bindS m k = \s0 -> let 
        (a, s1) = m s0 -- iteration argument and current state
        (b, s2) = k a s1 -- iterate to the new state. b can considered as the return value
        in (b, s2)
    --showS m = let (a, s1) = m 0
      --  in "{a:" ++ (show a) ++ ",s1:" ++ (show s1) ++ "}"

    
    --Monad S2 -- more close to class oriented languages
    data S2 this = This this
    unitS2 this = This this
    bindS2 (This this) k = (flip k) this

    --Stack using State Monad    
    pop :: [Int] -> (Int,[Int])
    pop (x:xs) = (x,xs)        
    push :: Int -> [Int] -> [Int]
    push a xs = a:xs

    
    x = show i5
    main = do        
        let mysum a b = Ok $ a + b
        let mydiv a b = if (b == 0) then Fail "Div by zero" else Ok $ a / b       

        putStrLn "MONAD E"
        --MONAD E
        let e1 = Ok 1    
        --e2 = Ok 1 + 1 does not compile because of the associassion
        --ther parser thinks that we want e2 = (Ok 1) + 1
        -- to solve this we can use the $ operator that just apply the function
        -- ($) :: (a -> b) -> a -> b
        let e2_1 = Ok $ 1 + 1
        -- or of course just parenthize correctly the expression
        let e2_2 = Ok (1 + 1)
        putStrLn $ showE e2_1
        -- bindE will call mysum because bindE of
        -- OK is just a postfix application
        putStrLn $ showE $ e2_1 `bindE` (mysum 1)
         
        let e3 = mydiv 1 0    
        putStrLn $ showE e3
        -- bindE wil NOT call mysum because bindE of
        -- FAIL is short-circuit
        putStrLn $ showE $ e3 `bindE` (mysum 1)

        putStrLn "MONAD E"
        --MONAD S - Stack without bind
        let stacka1 = [1,2,3]
        let (one, stacka2) = pop stacka1
        let (two, stacka3) = pop stacka2
        let (three, stacka4) = pop stacka3
        putStrLn (show one)
        putStrLn (show two)
        putStrLn (show three)
        --MONAD S - Stack with bind   
        
        let stackb1 = [1,2,3]
        let qtd = \s -> ()
        -- first we lift the push argument "i" to the state monad
        -- then we bind the lifted value wit hthe iteration  function
        let push2stack i = (unitS ()) `bindS` (\a -> push i)
        let pop2stack = (unitS ()) `bindS` (\a xs -> pop xs)
        let alg = do  
            a <- pop2stack
            b <- pop2stack
            return b
        putStrLn $ show $ alg stackb1


        --
        -- let push2stackb1 = (unitS2 stackb1) `bindS2` push
        -- let pop2stackb1 = (unitS2 stackb1) `bindS2` pop
        -- let ((),stackb3) = push2stackb1 13
        -- let (thirteen, stackb4) = pop2stackb1
        -- putStrLn (show thirteen)
       
        

