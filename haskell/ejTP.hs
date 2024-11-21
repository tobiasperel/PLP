import Data.Sequence (Seq(Empty))
import Data.Tree (Tree(subForest))
type Procesador a b = a ->[b] --a es el tipo de dato de entrada y b el de salida
data RoseTree a = Rose a [RoseTree a] deriving (Show)
data AT a = Nil | Node a (AT a) (AT a) (AT a) 
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving (Show)

--1
procVacio :: Procesador a b --que para cualquier estructura devuelve un resultado vac´ıo.
procVacio _ = []

procId :: Procesador a a -- que devuelve un unico resultado, que contiene la estructura original completa.
procId x = [x]

procCola :: Procesador [a] a
procCola (x:xs) = xs


miRT = Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 [], Rose 6 []]]

procHijosRose :: Procesador (RoseTree a) (RoseTree a) 
procHijosRose (Rose _ hijos) = hijos 

-- procHijosAT :: Procesador (AT a) (AT a)
-- procHijosAT Nil = []
-- procHijosAT (Node _ hijo1 hijo2 hijo3) = filter (\ x ->  x /= Nil  ) [hijo1, hijo2, hijo3]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie (TrieNodo valor _) = [valor]
trieEjemplo = TrieNodo (Just "Raiz") [('a', TrieNodo Nothing []), ('b', TrieNodo (Just "Nodo B") [])]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo valor hijos) = hijos

--2

foldRoseTree :: (a -> [b] -> b) -> RoseTree  a -> b 
foldRoseTree f (Rose nodo hijos) = f nodo (map (foldRoseTree f) hijos)

foldAT :: (a -> b -> b-> b -> b) ->b->  AT a ->b
foldAT f z Nil = z
foldAT f z (Node nodo hijo1 hijo2 hijo3) = f nodo (foldAT f z hijo1) (foldAT f z  hijo2) (foldAT f z hijo3)



--3
unoxuno :: Procesador [a] [a] --que devuelve cada elemento de la lista en una lista singleton.
unoxuno = map(\x -> [x])

--E.g., unoxuno [3,1,4,1,5,9] ⇝ [[3],[1],[4],[1],[5],[9]]

foldr' :: ( a -> b ->b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

sufijos :: Procesador [a] [a] --que devuelve todos los sufijos de la lista que se esta procesando.
sufijos = foldr (\x acc -> (x:head acc) :  acc ) [[]]
-- subfijos "plp" -> foldr(\x acc -> (x:head acc) : acc) [[]] "plp" -> foldr(\x acc -> (x:head acc) : acc) "p" -> foldr(\x acc -> (x:head acc) : acc) "lp" -> foldr(\x acc -> (x:head acc) : acc) "Plp" -> foldr(\x acc -> (x:head acc) : acc) "Plp" -> ["Plp", "lp", "p", ""]

--4

preorder:: AT a -> [a]
preorder Nil= []
preorder (Node nodo hijo1 hijo2 hijo3) = [nodo] ++ preorder hijo1 ++  preorder hijo2 ++  preorder hijo3


preorder' :: AT a -> [a]
preorder' = foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) []

postorder:: AT a -> [a]
postorder Nil= []
postorder (Node nodo hijo1 hijo2 hijo3) = postorder hijo1 ++  postorder hijo2 ++  postorder hijo3 ++ [nodo]


postorder' :: AT a -> [a]
postorder' = foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) []

inorder:: AT a -> [a]
inorder Nil= []
inorder (Node nodo hijo1 hijo2 hijo3) = inorder hijo1 ++  inorder hijo2 ++  [nodo] ++ inorder hijo3 

inorder' :: AT a -> [a]
inorder' = foldAT (\r h1 h2 h3 -> h1++h2++[r]++h3) []

miAT = Node 16 (Node 1 (Node 9 (Nil) (Nil) (Nil)) (Node 7 (Nil) (Nil) (Nil)) ((Node 2 )(Nil) (Nil) (Nil) ))  (Node 14 (Node 0 (Nil) (Nil) (Nil)) (Node 3 (Nil) (Nil) (Nil)) ((Node 6 )(Nil) (Nil) (Nil) )) (Node 10 (Node 8 (Nil) (Nil) (Nil)) (Node 5 (Nil) (Nil) (Nil)) ((Node 4 )(Nil) (Nil) (Nil) ))

--5 

preorderRose :: Procesador (RoseTree a) a
preorderRose (Rose nodo hijos) = nodo : concatMap preorderRose hijos 

preorderRose' :: Procesador (RoseTree a) a
preorderRose'  =  foldRoseTree (\ nodo hijos -> nodo : concat hijos)

hojasRose :: Procesador (RoseTree a) a 
hojasRose = foldRoseTree (\ nodo hijos -> if null hijos then nodo : concat hijos else concat hijos )

ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRoseTree (\nodo hijos -> if null hijos then [[nodo]] else map (nodo :) (concat hijos))


--6 
-- Definir la funci´on palabras que, dado un Trie, devuelva las palabras que incluye. Una palabra
-- estar´a incluida si y solo si en el nodo que termina un camino desde la ra´ız se encuentra un valor
-- que no sea Nothing. No se deben incluir palabras repetidas.

mapSnd :: (a -> b) -> [(c,a)] -> [(c,b)]
mapSnd f [] = []
mapSnd f ((x,y):xs) = (x, f y):mapSnd f xs

foldTrie :: (Maybe a -> [(Char,b)] -> b) -> Trie a -> b
foldTrie f (TrieNodo nodo ramas) = f nodo (mapSnd (foldTrie f) ramas)

caminos :: Trie a -> [String]
caminos = foldTrie f
  where
    -- f :: Maybe a -> [(Char, [String])] -> [String]
    f nodo hijos =
      -- Si el nodo tiene un valor, incluimos la cadena vacía como camino
      let caminoActual = if nodo == Nothing then [] else [""]
          -- Concatenamos los caracteres de las ramas a cada camino de los hijos
          caminosHijos = concatMap (\(c, caminosHijo) -> map (c:) caminosHijo) hijos
      in caminoActual ++ caminosHijos

palabras :: Trie a -> [String]
palabras (TrieNodo valor hijos) = case valor of
    Just x -> [x] ++ concatMap (\(c, t) -> map (c:) (palabras t)) hijos
    Nothing -> concatMap (\(c, t) -> map (c:) (palabras t)) hijos
--7

--8
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc condicion proc1 proc2 = \x -> if condicion x then proc1 x else proc2 x

(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) proc1 proc2 = \x -> proc1 x  ++ proc2 x

(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) proc1 proc2 = \x -> concatMap proc1 (proc2 x)
--((\z->[0..z]) .! (map (+1))) [1,3] -> [0,1,2,0,1,2,3,4]