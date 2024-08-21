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
factorial n = n * factorial (n - 1)

-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1 / x)

aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right x)
  | x == True = 1
  | otherwise = 0

limpiar :: String -> String -> String
limpiar _ [] = [] -- Si la segunda cadena es vacía, la función retorna una cadena vacía.
limpiar chars (x : xs)
  | x `elem` chars = limpiar chars xs -- Si el caracter `x` está en `chars`, se elimina.
  | otherwise = x : limpiar chars xs -- Si no está en `chars`, se mantiene y se continúa con la recursión.

sumaTotal :: [Float] -> Float
sumaTotal [] = 0
sumaTotal (x : xs) = x + sumaTotal xs

promedio :: [Float] -> Float
promedio x = sumaTotal x / fromIntegral (length x)

difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio xs = difPromedioAux xs (promedio xs)

difPromedioAux :: [Float] -> Float -> [Float]
difPromedioAux [] _ = []
difPromedioAux (x : xs) prom = (x - prom) : difPromedioAux xs prom

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x : xs) = x == head xs && todosIguales xs

data AB a = Nil | Bin (AB a) a (AB a)
--Bin (Bin Nil 2 Nil) 5 (Bin Nil 7 Nil)
-- arbol con nodo central 5, con hijo izquierdo 2 y derecho 7

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq x der) = Bin (negacionAB izq) (not x) (negacionAB der)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq x der) = x * productoAB izq * productoAB der
 -- productoAB (Bin (Bin Nil 2 Nil) 5 (Bin Nil 7 Nil))
