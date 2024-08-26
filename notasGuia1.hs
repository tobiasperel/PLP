curry1 :: ((a, b) -> c) -> a -> b -> c
curry1 f x y = f (x, y)

addPair :: (Int, Int) -> Int
addPair (x, y) = x + y

addCurried :: Int -> Int -> Int
addCurried = curry1 addPair

map1:: (a->b) -> [a] -> [b]
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs

-- La función map1 toma una función f de tipo (a->b) y devuelve otra función que toma una lista de tipo [a]. Esta función resultante, a su vez, toma la lista [a] y devuelve una lista de tipo [b].
-- map1 (+1) [1,2,3,4,5]
--map1 (curry1 addPair 7) [1,2,3,4,5]
 --usar la funcion siempre verdad para el filter1

siempreVerdad :: Int -> Bool
siempreVerdad x = True

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x : xs) = if p x then x : filter1 p xs else filter1 p xs

-- La función filter1 toma una función p de tipo (a->Bool) y devuelve otra función que toma una lista de tipo [a]. Esta función resultante, a su vez, toma la lista [a] y devuelve una lista de tipo [a].
-- filter1 (>3) [1,2,3,4,5]
-- filter1 (curry1 addPair 7) [1,2,3,4,5]
 
foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f z [] = z
foldr2 f z (x:xs) = f x (foldr2 f z xs)

-- La función foldr1 toma una función f de tipo (a->b->b), un valor z de tipo b y una lista de tipo [a]. Devuelve un valor de tipo b.
-- flodr2 (+) 0 [1,2,3,4,5]

suma :: [Int] -> Int
suma = foldr2 (+) 0

patito :: Int -> Int -> Int
patito x y = 2*x + y
-- foldr2 patito 0 [15, 2]
-- patito 15 (foldr2 patito 0 [2])
-- patito 15 (patito 2 (foldr2 patito 0 []))
-- patito 15 (patito 2 0)
-- patito 15 4
-- 34

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = foldr2(\ x rec -> rec ++ [x] ) [] (x:xs)
--reverse1 [1,2,3,4]
-- foldr2(\ x recaaaa -> recaaaa ++ [x] ) [] [1,2,3,4]
-- (\ x recaaaa -> recaaaa ++ [x] ) 1 (foldr2(\ x recaaaa -> recaaaa ++ [x] ) [] [2,3,4])
-- (\ x recaaaa -> recaaaa ++ [x] ) 1 (\ x recaaaa -> recaaaa ++ [x] ) 2 (foldr2(\ x recaaaa -> recaaaa ++ [x] ) [] [3,4])
-- (\ x recaaaa -> recaaaa ++ [x] ) 1 (\ x recaaaa -> recaaaa ++ [x] ) 2 (\ x recaaaa -> recaaaa ++ [x] ) 3 (foldr2(\ x recaaaa -> recaaaa ++ [x] ) [] [4])
-- (\ x recaaaa -> recaaaa ++ [x] ) 1 (\ x recaaaa -> recaaaa ++ [x] ) 2 (\ x recaaaa -> recaaaa ++ [x] ) 3 (\ x recaaaa -> recaaaa ++ [x] ) 4 (foldr2(\ x recaaaa -> recaaaa ++ [x] ) [] [])
-- (\ x recaaaa -> recaaaa ++ [x] ) 1 (\ x recaaaa -> recaaaa ++ [x] ) 2 (\ x recaaaa -> recaaaa ++ [x] ) 3 (\ x recaaaa -> recaaaa ++ [x] ) 4 []
-- (\ x recaaaa -> recaaaa ++ [x] ) 1 (\ x recaaaa -> recaaaa ++ [x] ) 2 (\ x recaaaa -> recaaaa ++ [x] ) 3 [4]
-- (\ x recaaaa -> recaaaa ++ [x] ) 1 (\ x recaaaa -> recaaaa ++ [x] ) [3,4]
-- (\ x recaaaa -> recaaaa ++ [x] ) 1 [3,4,2]
-- [3,4,2,1]

zip2 :: [a] -> [b] -> [(a, b)]
zip2 [] _ = []
zip2 _ [] = []
zip2 (x:xs) (y:ys) =  foldr2 (\(a, b) rec -> rec ++ [(a, b)]) [] (zip2 xs ys)


--tipos de datos algebraicos

data Persona = LaPersona String String Int
nombre, apellido :: Persona -> String
edad :: Persona -> Int
nombre (LaPersona n _ _) = n
apellido (LaPersona _ a _) = a
edad (LaPersona _ _ e) = e
tobiasPerel = LaPersona "Tobias" "Perel" 22
--nombre tobiasPerel

data Forma = Rectangulo Float Float | Circulo Float

radio :: Forma -> Float
radio (Circulo r) = r

area :: Forma -> Float
area (Rectangulo b h) = b * h
area (Circulo r) = pi * r^2
miRectangulo = Rectangulo 2 3
--area miRectangulo
miCirculo = Circulo 5
--area miCirculo
--radio miCirculo

data Nat = Zero | Succ Nat --Succ es el sucesor de un número natural

-- Definir la instancia de Show para imprimir valores de Nat
instance Show Nat where
    show Zero = "Zero"
    show (Succ Zero) = "uno"
    show (Succ (Succ Zero)) = "dos"
    show (Succ (Succ (Succ Zero))) = "tres"
    show (Succ n) = "Succ (" ++ show n ++ ")"



-- Algunos ejemplos de valores de tipo Nat
uno :: Nat
uno = Succ Zero

dos :: Nat
dos = Succ uno

tres :: Nat
tres = Succ dos

doble :: Nat -> Nat
doble Zero = Zero
doble (Succ n) = Succ (Succ (doble n))
--doble (Succ Zero) = Succ (Succ Zero) = Succ(Uno) = Dos
--doble (Succ (uno)) = Succ (succ (doble uno)) = Succ (succ (succ (doble zero))) = Succ (succ (succ zero)) = Succ (succ (uno)) = Succ (dos) = Tres
infinito :: Nat
infinito = Succ infinito

--doble infinito
-- succ (succ (doble infinito)) = succ (succ (succ (doble infinito))) = succ (succ (succ (succ (doble infinito)))) = ...

data AB a = Nil | Bin (AB a) a (AB a)

instance Show a => Show (AB a) where
    show Nil = "Nil"
    show (Bin izq x der) = "Bin (" ++ show izq ++ ") " ++ show x ++ " (" ++ show der ++ ")"

insertar :: Ord a => a -> AB a -> AB a
insertar x Nil = Bin Nil x Nil
insertar x (Bin iz y der) 
    | x < y = Bin(insertar x iz) y der
    | x > y = Bin iz y (insertar x der)
    |otherwise = Bin iz y der
-- Ord a => signfiica que a es un tipo de dato ordenable, nada mas
-- Despúes dice que toma un dato de tipo a y un arbol de tipo a y devuelve un arbol de tipo a
--insertar 5 (Bin Nil 3 Nil)
--insertar 5 (Bin (Bin Nil 3 Nil) 7 Nil)