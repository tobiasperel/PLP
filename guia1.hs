import Text.Read.Lex (Number)
--preguntas
-- que seria f y como funciona bien
-- funciones anonimas

-- foldr2  (a -> b -> b) -> b -> [a] -> b
-- foldr2 f z [] = z
-- foldr2 f z (x:xs) = f x (foldr2 f z xs)

--foldr (+) 0 [1, 2, 3, 4]
-- = (+) 1 (foldr (+) 0 [2, 3, 4])
-- = (+) 1 ( (+) 2 (foldr (+) 0 [3, 4]) )
-- = (+) 1 ( (+) 2 ( (+) 3 (foldr (+) 0 [4])) )
-- = (+) 1 ( (+) 2 ( (+) 3 ( (+) 4 0 )))
-- = (+) 1 ( (+) 2 ( (+) 3 4 ))
-- = (+) 1 ( (+) 2 7 )
-- = (+) 1 9
-- = 10


--foldr (\x y -> x + y) 0 [1, 2, 3, 4]
sum2 :: Int -> Int  -> Int
sum2 x y = foldr (+) 0 [x,y]

iguales :: Eq a => a -> a -> Bool
iguales x y =  if x == y then True else False 

elem2 :: Eq a => a -> [a] -> Bool
-- elem2 _ [] = False
elem2 n xs = foldr (\x rec -> (x == n) || rec) False xs -- aca puedo no usar xs

map':: (a -> b) ->  [a] -> [b]
map' f [] = []
-- map' f (x:xs) = f x : map' f xs
map' f xs  = foldr (\x rec -> f x: rec) [] xs
-- rec es el resutado de aplicar la recursion anteriormente. Al principio en una lista vacia y poco a poco se va llenando.
-- map' (\x -> 2*x) [1,2,3]

filter' :: (a-> Bool) -> [a] -> [a]
filter' f [] = []
filter' f  xs = foldr (\x rec -> if f x then x : rec else rec) [] xs

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f (x:xs) = foldr (\y rec -> if f y rec then y else rec) x xs

sumasParciales :: Num a => [a] -> [a]
sumasParciales = tail . foldl (\acc x -> acc ++ [last acc + x]) [0] 

sumaAlt :: Num a => [a] -> a
sumaAlt  = foldr (\x acc -> x - acc) 0 
--sumaAlt [1,2,3,4] --> 4-3-2-1

sumaAlt' :: Num a => [a] -> a
sumaAlt' = foldl (\x acc -> acc - x) 0 

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = 
    if null xs
    then [x]
    else x : elementosEnPosicionesPares (tail xs)

elementosEnPosicionesPares' :: [a] -> [a]
elementosEnPosicionesPares' xs = foldr(\ (i, x) rec -> if even i then x:rec else rec) [] (zip [0..] xs) 
-- la funcion zip combina para que acepte tuplas.  Por ejemplo, zip [0..] [a, b, c, d] produce [(0, a), (1, b), (2, c), (3, d)]

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> 
    if null ys
    then x : entrelazar xs []
    else x : head ys : entrelazar xs (tail ys)

entrelazar' :: [a] -> [a] -> [a]
entrelazar' (x:xs) ys = foldr (\(x, y) rec -> x : y : rec) [] (zip xs ys) 

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)
-- recr (\x xs rec -> x : rec) [10] [1,2,3,4] --> [1,2,3,4,10]

sacarUna' :: Eq a => a -> [a] -> [a]
sacarUna' n [] = []
sacarUna' n (x:xs) = if n == x then xs else x: sacarUna' n xs

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado n [] = []
insertarOrdenado n (x:xs) = if n > x then x: insertarOrdenado n xs else n:x:xs

insertarOrdenado' :: Ord a => a -> [a] -> [a]
insertarOrdenado' n [] = []

map'' :: (a->b) -> [a] ->[b]
map'' _ [] = []
map'' f (x:xs)  = f x : map'' f xs 


mapPares :: (a->b) -> [(a,a)] -> [(b,b)]
mapPares _ [] = []
mapPares f ((x,y):xs) = (f x , f y) : mapPares f xs 

--zip
armarPares :: [a] -> [b] -> [(a,b)]
armarPares [] [] = []
armarPares (x:xs) [] = []
armarPares [] (x:xs)  = []
armarPares (x:xs) (y:ys) = (x,y) : armarPares xs ys

--zipWith
--mapDoble (+) [1, 2, 3] [4, 5, 6]
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble _ [] [] = []
mapDoble _ [] (x:xs) = []
mapDoble _ (x:xs) [] = []
mapDoble f  (x:xs) (y:ys) = f x y : mapDoble f xs ys 


sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat [] [] = [] 
sumaMat (x:xs) (y:ys) = (zipWith (+) x y) : sumaMat xs ys -- se puede usar mapDoble

trasponer :: [[a]] -> [[a]]
trasponer [] = []
trasponer ([]:_) = []
trasponer xss = map head xss : trasponer (map tail xss)


foldNat :: a -> (Integer -> a -> a) -> Integer -> a
foldNat base _ 0 = base
foldNat base f n = f n (foldNat base f (n - 1))
--foldNat 0 (+) 5 --> 15 --> (+) 5 (fold Nat 0 f 4) --> (+) 5 ((+) 4 fold(nat 0 f 3))

data Polinomio a = X
        | Cte a
        | Suma (Polinomio a) (Polinomio a)
        | Prod (Polinomio a) (Polinomio a)

polinomio1 :: Polinomio a
polinomio1 = X

polinomio2 :: Polinomio Integer
polinomio2 = Cte 2

polinomio3 :: Polinomio Integer
polinomio3 = Suma X (Cte 5)

polinomio4 :: Polinomio Integer
polinomio4 = Prod X (Suma X (Cte 5))

evaluar :: Num a => a -> Polinomio a -> a
evaluar x X = x
evaluar _ (Cte a) = a
evaluar x (Suma p1 p2) = evaluar x p1 + evaluar x p2
evaluar x (Prod p1 p2) = evaluar x p1 * evaluar x p2

data AB a = Nil | Bin (AB a) a (AB a)
--Usando recursión explícita, definir los esquemas de recursión estructural (foldAB) y primitiva (recAB), y
-- dar sus tipos
foldAB :: (b -> c -> c) -> c -> AB b -> c
foldAB f z Nil = z
foldAB f z (Bin iz x der) = f x (foldAB f (foldAB f z iz ) der ) 

sumarElementos :: Num a => AB a -> a
sumarElementos = foldAB (+) 0
--sumarElementos (Bin (Bin Nil 4 Nil) 3 Nil)
-- foldAb (+) 0 (Bin (Bin Nil 4 Nil) 3 Nil) --> (+) 3 (foldAB (+) (foldAB (+) 0 Nil) Nil) --> (+) 3 (foldAB (+) 0 Nil) --> (+) 3 0 --> 3

recAb :: (b-> c -> c) -> c -> AB b -> c 
recAb f z Nil = z
recAb f z (Bin iz x der) = f x (recAb f (recAb f z iz) der)

esNil:: AB a -> Bool
esNil = recAb (\ z arbol -> False) True  -- si no se va al recursivo devielñve z qie es True

altura :: Num a => AB a -> a
altura = recAb ( \ z arbol  -> z+1) 0 
-- altura (Bin (Bin Nil 4 Nil) 3 Nil) --> recAb ( \ z arbol  -> z+1) 0 (Bin (Bin Nil 4 Nil) 3 Nil) --> 1 + recAb ( \ z arbol  -> z+1) 0 (Bin Nil 4 Nil) --> 1 + 1 + recAb ( \ z arbol  -> z+1) 0 Nil --> 1 + 1 + 0 --> 2