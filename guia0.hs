valorAbsoluto :: Float -> Float
valorAbsoluto x 
    | x >= 0 = x
    | otherwise = -x


bisiesto :: Int -> Bool
bisiesto n 
    | mod n 400 == 0 = True
    | mod n 100 == 0 = False
    | mod n 4 == 0 = True
    | otherwise = False

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

--data Maybe a = Nothing | Just a
--data Either a b = Left a | Right b

inverso:: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right x) 
  | x == True = 1
  | otherwise = 0

limpiar :: String -> String -> String
limpiar _ [] = []  
limpiar xs (y:ys)
  | y `elem` xs = limpiar xs ys  -- Si el carácter `y` está en `xs`, lo omitimos
  | otherwise   = y : limpiar xs ys  -- Si no está en `xs`, lo conservamos y seguimos
