module Main where
    --Identity monad
    type I a = a
    --Identity Function
    unitI :: a -> a
    unitI a = a
    -- postfix application
    bindI :: a -> (a -> b) -> b
    bindI a f = f a
    (<|) :: a -> (a -> b) -> b
    (<|) a f = f a 
    --showI :: a -> String
    showI a = show a

    f = \x -> x*2
    i1 = unitI 1
    i2 = i1 `bindI` f
    i3 = (unitI 1) `bindI` f
    i4 = 1 <| f
    x = showI i4
    main = putStrLn x