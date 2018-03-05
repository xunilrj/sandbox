import Data.Maybe

id :: a -> a
id x = x
f :: Float -> Int
f x = round x
g :: Int -> String
g x = show x
composed :: Float -> String
composed = g . f
compose :: (a->b) -> (b->c) -> (a->c)
compose f g = g . f
-- not working
mem :: (()->a) -> () -> a
mem f = 
    \() -> 
        do  
            value <- get
            case value of 
                Just v -> return v
                Nothing ->
                    put $ Some value
                    return value
                    where value = f ()
main = do
    print $ composed (Main.id 11.0)
    let f1 = compose Main.id f
    let f2 = compose f Main.id
    print $ (f1 1) == (f2 1)
    let slowf = 65
    let memslowf = mem slowf
    print $ memslowf ()