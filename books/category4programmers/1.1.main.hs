f :: Float -> Int
f x = round x
g :: Int -> String
g x = show x
composed :: Float -> String
composed = g . f
main = print $ composed 11.0