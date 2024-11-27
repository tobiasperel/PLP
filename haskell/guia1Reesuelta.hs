
curry1 :: ((a,b)->c) -> a -> b-> c
curry1  f x y = f (x,y)

uncurry1 :: (a -> b-> c) -> ((a,b)->c)
uncurry1 f (x,y) = f x y

-- foldr2  (a -> b -> b) -> b -> [a] -> b
-- foldr2 f z [] = z
-- foldr2 f z (x:xs) = f x (foldr2 f z xs)

suma :: Num a => a -> a -> a
suma x y = x + y

sumaFold :: Num b => b -> b -> b
sumaFold x y = foldr (\x y -> x+y) 0 [x,y]

elemento ::  Eq a =>  a -> [a] -> Bool
elemento _ [] = False
elemento x (y:ys) = x == y || elemento x ys

elementoFold ::  Eq a =>  a -> [a] -> Bool
elementoFold x (y:ys) = foldr (\y rec -> x==y || rec) False (y:ys)

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f [] = []
filter2 f (x:xs) = if f x then x:filter2 f xs else filter2 f xs

foldFilter :: (a->Bool) -> [a] -> [a]
foldFilter f (y:ys) = foldr (\x rec -> if f x then x:rec else rec) [] (y:ys)

map2 :: (a->b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x : map2 f xs

mapFold :: (a->b) -> [a] -> [b]
mapFold f (x:xs) = foldr (\y rec -> f y : rec) [] (x:xs)


mejorSegun :: Num a => (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr (\x rec -> if f x rec then x else rec) 0

sumasParciales :: (Num a, Eq a) => [a] -> [a]
sumasParciales (y:ys)= foldl (\rec x -> rec ++ [last rec + x]) [y] ys

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

sumaAlt' :: Num a => [a] -> a
sumaAlt' = foldl (-) 0

entrelazar2 :: [a] -> [a] -> [a] 
entrelazar2 = foldr (\x rec -> \  ys ->
        if null ys then x : rec []
        else x : head ys : rec (tail ys)
    ) id

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)    

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr(\x xs rec -> if e == x then xs else x:rec) []

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr(\x xs rec -> if e < x then e:x:xs else x:rec ) []

patito :: (Num a, Eq a) => [a] -> a
patito = foldr(\x rec -> if x == 3 then 3 else  x + rec) 0 

--patito [1,2,3,4,5]
--foldr(\x rec -> if x == 3 then 3 else  x + rec) 0 [1,2,3,4,5]
--1 + foldr(\x rec -> if x == 3 then 3 else  x + rec) 0 [2,3,4,5]
--1 + 2 + foldr(\x rec -> if x == 3 then 3 else  x + rec) 0 [3,4,5]
--1 + 2 + 3

mapPares :: (a->b) -> [(a,a)] -> [(b,b)]
mapPares f = foldr (\(x,y) rec -> (f x, f y) : rec) [] 

armarPares :: [a] -> [b] -> [(a,b)]
armarPares = foldr(\x rec -> \ys -> if null ys then [] else (x,head(ys)):rec(tail ys)) (const [])

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\x rec -> \ys' -> if null ys' then [] else f x (head ys') : rec(tail ys') ) (const []) 

foldNat:: (Integer -> b -> b) -> b -> [Integer] ->b
foldNat _ z [] =  z
foldNat f z (x:xs) = f x (foldNat f z xs)

data AB a = Nil | Bin (AB a) a (AB a)
miAb = Bin (Bin (Bin Nil 1 Nil) 2 (Bin Nil 30 Nil)) 4 (Bin Nil 5 Nil)

foldAB :: b -> (b ->  a  -> b ->  b) -> AB a ->  b
foldAB casoNil _ Nil = casoNil
foldAB casoNil casoBin (Bin iz x der) = casoBin (rec iz) x (rec der)  
    where rec = foldAB casoNil casoBin

recAB :: b -> (AB a -> b -> a -> b -> b) -> AB a -> b
recAB casoNil _ Nil = casoNil 
recAB casoNil casoBin (Bin iz x der) = casoBin (Bin iz x der) (rec iz) x (rec der)
    where rec = recAB casoNil casoBin 


esNil :: AB a -> Bool
esNil x = case x of
    Nil -> True
    _ -> False

altura :: AB a -> Integer
altura = foldAB 0 (\recIz x recDer -> max recIz recDer +1 )

cantNodos :: AB a -> Integer
cantNodos = foldAB 0 (\recIz x recDer -> recIz + recDer +1 )

mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB f (Bin iz x der) = foldAB x (\recIz x recDer -> if f recIz x then (if f recIz recDer then recIz else recDer) else (if f x recDer then x else recDer)) (Bin iz x der)

-- esABB :: Ord a => AB a -> Bool
-- esABB = recAB True (\arb recIz x recD -> )

data AIH a = Hoja a | Bin' (AIH a) (AIH a)
miAIH = Bin' (Hoja 1) (Bin' (Hoja 2) (Bin' (Hoja 3) (Bin' (Hoja 4) (Hoja 5))))

foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH casoHoja _ (Hoja x) = casoHoja x
foldAIH casoHoja casoBin (Bin' iz der) = casoBin (foldAIH casoHoja casoBin iz) (foldAIH casoHoja casoBin der) 

alturaAIH :: AIH a -> Integer
alturaAIH = foldAIH (const 1) (\alturaIzq alturaDer -> 1 + max alturaIzq alturaDer)

data RoseTree a = Rose a [RoseTree a]
miRT = Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 [], Rose 6 []]]

foldRoseTree :: (a -> [b] -> b) -> RoseTree a -> b
foldRoseTree f (Rose x hijos) = f x (map (foldRoseTree f) hijos)

hojasRose :: RoseTree a -> Integer
hojasRose = foldRoseTree(\_ recHijos -> if null recHijos then 1 else sum recHijos ) 

distancias :: RoseTree a -> [Int]
distancias = foldRoseTree(\_ recHijos -> if null recHijos then [0] else concatMap (map (+1)) recHijos)

alturaRose :: RoseTree a -> Integer
alturaRose = foldRoseTree(\_ recHijos  -> if null recHijos then 0 else 1 + maximum recHijos) 