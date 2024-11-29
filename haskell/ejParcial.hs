import Data.List (union)

data Buffer a = Empty | Write Int a (Buffer a )| Read Int (Buffer a)

-- foldBuffer :: b -> (Int -> a -> b-> b) -> (Int -> b -> b) -> Buffer a -> b
-- foldBuffer casoEmpty _ _  Empty = casoEmpty
-- foldBuffer casoEmpty casoWrite casoRead (Write num elem resto) = casoWrite num elem (foldBuffer casoEmpty casoWrite casoRead resto)
-- foldBuffer casoEmpty casoWrite casoRead (Read num resto) = casoRead num (foldBuffer casoEmpty casoWrite casoRead resto)

buf = Write 1 "a" $ Write 2 "b" $ Write 1 "C" $ Empty

foldBuffer :: b -> (Int -> a -> b-> b) -> (Int -> b -> b) -> Buffer a -> b
foldBuffer casoEmpty casoWrite casoRead t = case t of 
     Empty ->  casoEmpty
     Write num elem resto -> casoWrite num elem (rec resto)
     Read num resto -> casoRead num (rec resto)
    where rec = foldBuffer casoEmpty casoWrite casoRead 

recBuffer :: b -> (Buffer a -> Int -> a -> b ->b) -> (Buffer a -> Int -> b -> b) -> Buffer a -> b
recBuffer casoEmpty casoWrite casoRead t = case t of
    Empty ->  casoEmpty
    Write num elem resto -> casoWrite resto num elem (rec resto)
    Read num resto -> casoRead resto num (rec resto)
    where rec = recBuffer casoEmpty casoWrite casoRead 

posicionesOcupadas :: Buffer a -> [Int]
posicionesOcupadas = foldBuffer [] (\n _ rec -> union [n] rec ) (\n rec -> filter (/=n) rec )

contenido :: Int -> Buffer a -> Maybe a
contenido n1 = foldBuffer Nothing (\n2 elem rec -> if n1 == n2 then Just elem else rec ) (\n2 rec -> if n1 == n2 then Nothing else rec ) 

puedeCompletarLectura :: Buffer a -> Bool 
puedeCompletarLectura = recBuffer True (\ buf n _ rec -> rec ) ( \buf n rec -> elem n (posicionesOcupadas buf) && rec) 

deshacer :: Buffer a -> Int -> Buffer a
deshacer = recBuffer (const Empty) (\ buf n el rec -> \i -> if i == 0 then (Write n el buf) else rec (i-1) ) (\ buf n rec -> \i -> if i ==0 then (Read n buf) else rec(i-1))

data AT a = NilT | Tri a (AT a) (AT a) (AT a)
miAT = Tri 1 (Tri 2 NilT NilT NilT) (Tri 3 NilT NilT NilT) (Tri 4 NilT NilT NilT)
foldAT :: b -> (a-> b-> b-> b->b) -> AT a -> b
foldAT casoNil _ NilT = casoNil
foldAT casoNil casoTri (Tri x iz med der) = casoTri x (rec iz) (rec med) (rec der)
    where rec = foldAT casoNil casoTri 

recAT :: b -> (AT a -> AT a -> AT a-> a-> b -> b-> b->b) -> AT a -> b 
recAT casoNil _ NilT= casoNil
recAT casoNil casoTri (Tri x iz med der) = casoTri iz med der x (rec iz) (rec med) (rec der)
    where rec = recAT casoNil casoTri 

preorder :: AT a -> [a]
preorder = foldAT [] (\x recIz recMed recDer -> [x] ++ recIz ++ recMed ++ recDer) 

mapAT :: (a->b)-> AT a -> AT b 
mapAT f = foldAT NilT (\x recIz recMed recDer -> Tri (f x) recIz recMed recDer)

nivel :: AT a -> Int -> [a]
nivel = foldAT (\_-> []) (\x recIz recMed recDer -> \l -> if l == 0 then [x] else recIz (l - 1) ++ recMed (l - 1) ++ recDer (l - 1))

data Prop = Var String | No Prop | Y Prop Prop | O Prop Prop | Imp Prop Prop
type Valuacion = String -> Bool

foldProp :: (String -> b) -> (b->b) -> (b-> b -> b) -> (b-> b -> b) -> (b-> b -> b) -> Prop -> b 
foldProp cV cN cY cO cI t = case t of 
    Var s -> cV s 
    No iz -> cN (rec iz)
    Y iz der -> cY (rec iz) (rec  der) 
    O iz der -> cO (rec iz) (rec  der)  
    Imp iz der -> cI (rec iz) (rec  der) 
    where rec = foldProp cV cN cY cO cI

recProp :: (String -> b) -> (Prop -> b->b) -> (Prop -> Prop -> b-> b -> b) -> (Prop -> Prop -> b-> b -> b) -> (Prop -> Prop -> b-> b -> b) -> Prop -> b 
recProp cV cN cY cO cI t = case t of 
    Var s -> cV s 
    No iz -> cN iz (rec iz)
    Y iz der -> cY iz der (rec iz) (rec  der) 
    O iz der -> cO iz der (rec iz) (rec  der)  
    Imp iz der -> cI iz der (rec iz) (rec  der) 
    where rec = recProp cV cN cY cO cI

--variables :: 