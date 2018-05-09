--6.2
--6.3
--6.4
module Main where
    data Shape = Circle Float | Rect Float Float | Square Float
    area :: Shape -> Float
    area (Circle r) = pi * r * r
    area (Rect w h) = w * h
    area (Square w) = w*w
    circ :: Shape -> Float
    circ (Circle r) = 2.0 * pi * r
    circ (Rect w h) = 2.0 * (w+h)
    circ (Square w) = 4.0 * w
    main = do
      putStrLn . show . area $ Circle 1
      putStrLn . show . circ $ Circle 1