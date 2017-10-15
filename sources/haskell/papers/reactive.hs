module Main where
    --Identity monad
    type I a = a
    --Identity Function
    unitI :: a -> a
    unitI a = a
    -- postfix application
    bindI :: a -> (a -> I b) -> I b
    bindI a f = f a
    (<|) :: a -> (a -> I b) -> I b
    (<|) a f = f a 
    --showI :: a -> String
    showI a = show a

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
    failE a = Fail a
    -- strict post fix (only when success)
    bindE :: E a -> (a -> E b) -> E b
    bindE (Ok a) f = f a
    bindE (Fail a) f = Fail a

    f = \x -> x*2
    i1 = unitI 1
    i2 = i1 `bindI` f
    i3 = (unitI 1) `bindI` f
    i4 = 1 <| f
    x = showI i4
    main = putStrLn x