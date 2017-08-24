type Parser a = String -> [(a, String)]

result :: a -> Parser a
result v inp = [(v,inp)]

zero :: Parser a
zero inp = []

item :: Parser Char
item inp = case inp of
  [] -> []
  (x:xs) -> [(x,xs)]
  
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f inp = concat [f v inp2 | (v,inp2) <- p inp]

sat :: (Char -> Bool) -> Parser Char
sat p = item `bind` \x ->
  if p x then result x else zero
  
char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

plus :: Parser a -> Parser a -> Parser a
plus p q inp = (p inp ++ q inp)

letter :: Parser Char
letter = lower `plus` upper 

alphanum :: Parser Char
alphanum = letter `plus` digit

--many :: Parser a -> Parser [a]
--many p inp = p inp

word :: Parser String
word = neWord `plus` result ""
  where neWord = letter `bind` \x ->
                 word `bind` \xs ->
                 result (x:xs)

main = do
  let p = do
    d1 <- digit "1"
    d2 <- digit "2"
    result (d1,d2)
  let r = p "12"
  print r