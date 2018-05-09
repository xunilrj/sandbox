-- Isomorphism for sum and product types
-- a+a = 2*a
module Main where
    aa2a :: Either a a -> (Bool,a)
    aa2a (Left x) = (False,x)
    aa2a (Right x) = (True, x)
    _2aaa :: (Bool, a) -> Either a a
    _2aaa (False, x) = Left x
    _2aaa (True, x) = Right x
    print2 :: Either a a -> String
    print2 (Left x) = "Left"
    print2 (Right x) = "Right"
    print3 :: (Bool,a) -> String
    print3 (False, x) = "False"
    print3 (True, x) = "True"
    main = do
      putStrLn . print3 . aa2a $ Left 1
      putStrLn . print3 . aa2a $ Right 1
      putStrLn . print2 . _2aaa $ (False, 1)
      putStrLn . print2 . _2aaa $ (True, 1)