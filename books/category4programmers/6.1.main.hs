--Maybe <-> Either isomotphism
module Main where
    maybe2Either :: Maybe a -> Either () a
    maybe2Either (Just x) = Right x
    maybe2Either Nothing = Left ()
    either2maybe :: Either () a -> Maybe a
    either2maybe (Left x) = Nothing
    either2maybe (Right x) = Just x
    
    printMaybe :: Maybe a -> IO ()
    printMaybe (Just x) = putStrLn "Just"
    printMaybe (Nothing) = putStrLn "Nothing"
    printEither :: Either () a -> IO()
    printEither (Left ()) = putStrLn "Left"
    printEither (Right x) = putStrLn "Right"
    
    main = do
      printMaybe . either2maybe $ Left ()
      printMaybe . either2maybe $ Right 1
      printEither . maybe2Either $ Nothing
      printEither . maybe2Either $ Just 1