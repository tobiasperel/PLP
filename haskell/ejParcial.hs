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
    Write num elem resto -> casoWrite (Write num elem resto) num elem (rec resto)
    Read num resto -> casoRead (Read num resto) num  (rec resto)
    where rec = recBuffer casoEmpty casoWrite casoRead 

posicionesOcupadas :: Buffer a -> [Int]
posicionesOcupadas = foldBuffer [] (\n _ rec -> union [n] rec ) (\n rec -> filter (/=n) rec )

contenido :: Int -> Buffer a -> Maybe a
contenido n1 = foldBuffer Nothing (\n2 elem rec -> if n1 == n2 then Just elem else rec ) (\n2 rec -> if n1 == n2 then Nothing else rec ) 

puedeCompletarLectura :: Buffer a -> Bool 
puedeCompletarLectura = recBuffer True (\ buf n _ rec -> rec ) ( \buf n rec -> elem n (posicionesOcupadas buf) && rec) 
