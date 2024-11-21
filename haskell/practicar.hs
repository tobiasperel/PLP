curry1 :: ((a, b ) -> c) -> a -> b -> c
curry1 f x y = f (x, y)

sumaNormal :: Num a => a -> a -> a
sumaNormal x y = x+y

sumFold :: Num a => a -> a -> a
sumFold x y = foldr ((+)) 0 [x,y]

elemNormal :: Eq a => a -> [a] ->Bool
elemNormal y xs = foldr (\ x -> (||) (y == x)) False xs

elemFoldr :: Eq a => a -> [a] -> Bool
elemFoldr y = foldr (\x rec-> x == y || rec) False

filterNormal :: (a -> Bool) -> [a] -> [a]
filterNormal f [] = []
filterNormal f (x:xs) = if f x then x : filterNormal f xs else filterNormal f  xs

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold f  = foldr (\x rec -> if f x then x: rec else rec) []

mapNormal :: (a->b)-> [a] -> [b]
mapNormal f xs = map f xs

mapFold ::  (a->b)-> [a] -> [b]
mapFold f = map (\ x -> f x)

mejorSegun :: Num a => (a -> a -> Bool) -> [a] -> a
mejorSegun f  = foldr (\x rec -> if f x rec then x else rec) 0

--  Denir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
-- otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
-- desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ; [1,5,4,4,9]

sumasParciales :: Num a => [a] -> [a]
sumasParciales (y:xs) = foldl (\rec x -> rec ++ [last rec + x]) [y] xs

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr ((-)) 0
-- sumaAlt [1,2,10] --> foldr f 0 [1,2,10]
-- -> f 1 (foldr f 0 [2,10]) --> f 1 ((f 2 foldr f 0 [10]))
-- f 1 ((f 2 (f 10 foldr f 0 []) --> f 1 ((f 2 (f 10 0) --> 
--f 1 (f 2 10)  --> f 1 -8 --> 9

armarPares :: [a] -> [b] -> [(a,b)]
armarPares [] [] = []
armarPares (x:xs) [] = []
armarPares [] (x:xs)  = []
armarPares (x:xs) (y:ys) = (x,y) : armarPares xs ys

armarParesFoldr :: [a] -> [b] -> [(a, b)]
armarParesFoldr = foldr (\x rec -> \ys -> if null ys then [] else (x,head ys) : rec(tail ys)) (const [] )

foldNat ::  (Integer -> b -> b) -> b -> [Integer] ->b
foldNat f z [] = z
foldNat f z (x:xs)  = f x (foldNat f z xs)

crearLista :: [Integer] -> Integer -> Integer -> [Integer]
crearLista l _ 0 = []
crearLista l n e = l ++ [n] ++ crearLista l n (e-1)

potencia :: Integer -> Integer -> Integer
potencia n e = foldNat (\x rec -> x*rec) 1 (crearLista [] n e)

data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b-> (b-> a -> b -> b) -> AB a -> b
foldAB casoNil casoBin Nil = casoNil
foldAB casoNil casoBin (Bin iz x der) = casoBin (foldAB casoNil casoBin iz ) x (foldAB casoNil casoBin der )


esNil :: AB a -> Bool
esNil x = case x of
    Nil -> True
    _ -> False

altura :: AB a -> Int
altura = foldAB 0 (\ recIz _ recDer -> 1 + max recIz recDer)
miAb = Bin (Bin (Bin Nil 1 Nil) 2 (Bin Nil 3 Nil)) 4 (Bin Nil 5 Nil)

cantNodos:: AB a -> Integer
cantNodos = foldAB 0 (\iz _ der -> 1+ iz + der)

mejorSegun'' :: (a -> a -> Bool) -> AB a -> a
mejorSegun'' f = foldAB (error "Empty tree") (\iz x der -> if f x (if f iz der then iz else der) then x else (if f iz der then iz else der))

data AIH a = Hoja a | Bin' (AIH a) (AIH a)

foldAIH :: b -> (b -> b -> b) -> AIH a -> b
foldAIH casoHoja casoBin (Hoja x) = casoHoja
foldAIH casoHoja casoBin(Bin' iz der) = casoBin (foldAIH casoHoja casoBin iz) (foldAIH casoHoja casoBin der)

alturaAIH :: AIH a -> Integer
alturaAIH = foldAIH 0 (\iz der -> 1+ max iz der)
-- alturaAIH (Bin' (Hoja 1) (Bin' (Hoja 2) (Bin' (Hoja 3) (Hoja 4)))) --> 3

data RoseTree a = Rose a [RoseTree a]


foldRoseTree :: (a-> [b]->b) -> RoseTree a  -> b
foldRoseTree f (Rose raiz hijos) = f raiz (map (foldRoseTree f) hijos) 
--foldRoseTree (\raiz hijos -> raiz : concat hijos) (Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 [], Rose 6 []]]) --> [1,2,3,4,5,6]
--concat [[1], [2], [3,4,5,6]] --> [1,2,3,4,5,6]

hojas :: RoseTree a  -> [a]
hojas = foldRoseTree(\raiz hijos -> raiz : concat hijos)

distancias :: RoseTree a  -> [Int]
distancias = foldRoseTree(\raiz hijos -> if null hijos then [0] else concatMap (map (+1)) hijos )

altura''' :: RoseTree a  -> Int
altura''' = foldRoseTree (\_ hijos -> if null hijos then 0 else 1 + maximoLista hijos 0 )  

maximoLista :: [Int]  -> Int -> Int
maximoLista [] maxi = maxi
maximoLista (x:xs) maxi = if x > maxi then maximoLista xs x else maximoLista xs maxi


data AT a = NilT | Tri a (AT a) (AT a) (AT a)

foldAT :: b -> (a-> b -> b ->b -> b) -> AT a -> b
foldAT casoNil casoBin NilT = casoNil
foldAT casoNil casoBin (Tri x iz med der) = casoBin x (foldAT casoNil casoBin iz) (foldAT casoNil casoBin med) (foldAT casoNil casoBin der)

preorder :: AT a -> [a]
preorder = foldAT [] (\ x recIz recMed recDer -> [x] ++ recIz ++ recMed ++ recDer  )

postorder :: AT a -> [a]
postorder = foldAT [] (\ x recIz recMed recDer -> recIz  ++ recMed ++ recDer ++ [x]  )
ejemploAT = Tri 16 (Tri 1 (Tri 9 NilT NilT NilT) (Tri 7 NilT NilT NilT) (Tri 2 NilT NilT NilT)) (Tri 14 (Tri 0 NilT NilT NilT) (Tri 3 NilT NilT NilT) (Tri 6 NilT NilT NilT)) (Tri 10 (Tri 8 NilT NilT NilT) (Tri 5 NilT NilT NilT) (Tri 4 NilT NilT NilT))

map' :: (a->b) -> [a] -> [b]
map' f [] = [] --l0
map' f (x:xs) = f x : (map' f xs) --l1

mapAT :: (a->b) -> AT a -> AT b
mapAT f NilT = NilT
mapAT f (Tri r iz med der) = Tri (f r) (mapAT f iz)  (mapAT f med)  (mapAT f der) 

instance Show a => Show (AT a) where
    show NilT = "NilT"
    show (Tri r iz med der) = "Tri " ++ show r ++ " " ++ show iz ++ " " ++ show med ++ " " ++ show der

nivel :: AT a -> Int -> [a]
nivel = foldAT (const []) (\x iz med der -> \i -> if i == 0 then [x] else iz (i-1) ++ med (i-1) ++ der (i-1) )

data Prop = Var String | No Prop | Y Prop Prop | O Prop Prop | Imp Prop Prop  
type Valuacion = String -> Bool

foldProp :: (String -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Prop -> b
foldProp f1 _ _ _ _ (Var x) = f1 x 
foldProp f1 f2 f3 f4 f5 (No x) =  f2 (foldProp f1 f2 f3 f4 f5 x)
foldProp f1 f2 f3 f4 f5 (Y x z) =  f3 (foldProp f1 f2 f3 f4 f5 x) (foldProp f1 f2 f3 f4 f5 z)
foldProp f1 f2 f3 f4 f5 (O x z) =  f4 (foldProp f1 f2 f3 f4 f5 x) (foldProp f1 f2 f3 f4 f5 z)
foldProp f1 f2 f3 f4 f5 (Imp x z) =  f5 (foldProp f1 f2 f3 f4 f5 x) (foldProp f1 f2 f3 f4 f5 z)

recProp ::  (String -> b) -> (Prop -> b->b ) -> (Prop -> Prop -> b -> b -> b ) -> (Prop -> Prop -> b -> b -> b ) -> (Prop -> Prop -> b -> b -> b ) -> Prop -> b
recProp f1 f2 f3 f4 f5 (Var x) = f1 x
recProp f1 f2 f3 f4 f5 (No x) = f2 x (recProp f1 f2 f3 f4 f5 x)
recProp f1 f2 f3 f4 f5 (Y x z) = f3 x z (recProp f1 f2 f3 f4 f5 x) (recProp f1 f2 f3 f4 f5 z)
recProp f1 f2 f3 f4 f5 (O x z) = f4 x z (recProp f1 f2 f3 f4 f5 x) (recProp f1 f2 f3 f4 f5 z)
recProp f1 f2 f3 f4 f5 (Imp x z) = f5 x z (recProp f1 f2 f3 f4 f5 x) (recProp f1 f2 f3 f4 f5 z)

evaluarProp :: Prop -> Bool
evaluarProp = foldProp (\_ -> False) not (&&) (||) (\x y -> not x || y)

miProp = Y (Var "p" ) (No (Y (Var "p") (No (Var "q"))))
 
variablesR :: Prop -> [String]
variablesR = foldProp (\x -> [x]) id (++) (++) (++)

variables :: Prop -> [String]
variables = sacarRepetidos . variablesR

sacarRepetidos :: Eq a => [a] -> [a]
sacarRepetidos [] = []
sacarRepetidos (x:xs) = x : sacarRepetidos (filter (/= x) xs)

evaluar'' :: Valuacion -> Prop -> Bool
evaluar'' val = foldProp val not (&&) (||) (\b1 b2 -> not b1 || b2) 

estaEnFNN :: Prop -> Bool 
estaEnFNN (Var _) = False
estaEnFNN (No (Var x) ) = True
estaEnFNN (No p) = False 
estaEnFNN (Y p1 p2) = estaEnFNN(p1) || estaEnFNN(p1)
estaEnFNN (O p1 p2) = estaEnFNN(p1) || estaEnFNN(p1)
estaEnFNN (Imp p1 p2) = estaEnFNN(p1) || estaEnFNN(p1)



data Componente = Contenedor | Motor | Escudo | Cañon deriving Eq
data NaveEspacial = Modulo Componente NaveEspacial NaveEspacial | Base Componente deriving Eq

instance Show Componente where
    show Contenedor = "Contenedor"
    show Motor = "Motor"
    show Escudo = "Escudo"
    show Cañon = "Cañon"

instance Show NaveEspacial where
    show (Base c) = show c
    show (Modulo c n1 n2) = "Modulo " ++ show c ++ " (" ++ show n1 ++ ") (" ++ show n2 ++ ")"


foldNave :: (Componente-> b) -> (Componente -> b -> b -> b )  -> NaveEspacial -> b
foldNave casoBase casoMod (Base c) = casoBase c
foldNave casoBase casoMod (Modulo c n1 n2) = casoMod c (foldNave casoBase casoMod n1 ) (foldNave casoBase casoMod n2 ) 
-- foldNave (\x -> 1) (\x iz der -> 1 + iz + der) (Modulo Contenedor (Base Motor) (Base Escudo)) --> 3

recNave:: (Componente -> b) -> (Componente -> NaveEspacial -> b -> NaveEspacial ->b -> b) -> NaveEspacial -> b
recNave casoBase casoMod (Base c) = casoBase c 
recNave casoBase casoMod (Modulo c n1 n2) = casoMod c n1 (recNave casoBase casoMod n1) n2 (recNave casoBase casoMod n2)


espejo :: NaveEspacial -> NaveEspacial
espejo = foldNave (\ c -> Base c)  (\x rec1 rec2 -> Modulo x rec2 rec1)
-- espejo (Modulo Contenedor (Base Motor) (Base Escudo)) --> Modulo Contenedor (Base Escudo) (Base Motor)

esSubnavePropia :: NaveEspacial -> NaveEspacial -> Bool
esSubnavePropia sub = recNave (\c -> False) (\c n1 rec1 n2 rec2 -> sub == Modulo c n1 n2 || rec1 || rec2|| sub == n1 || sub == n2)

truncar :: NaveEspacial -> Integer -> NaveEspacial
truncar = foldNave (\c i -> if i == 0 then Base c else Base c) (\c rec1 rec2 -> \i -> if i == 0 then Base c else Modulo c (rec1 (i-1) ) (rec2 (i-2) ) )

alturaFol :: AIH a -> Integer
alturaFol = foldAIH 0 (\ recIz recDer -> 1 + max recIz recDer  )

listaAIH :: [AIH ()]
listaAIH = iterate nextAIH (Hoja ())
  where
    nextAIH tree = Bin' tree tree
-- listaAIH !! 3

-- Either es un tipo de suma que ya existe en Haskell:
-- data Either a b = Left a | Right b

evaluate :: Either Int Bool -> String
evaluate value = case value of
    Left x -> if x == 0
              then "x is zero"
              else "x is not zero"
    Right y -> if y
               then "y is True"
               else "y is False"

-- Ejemplos de uso
-- example1 = evaluate (Left 0)    -- Esto devolverá: "x is zero"