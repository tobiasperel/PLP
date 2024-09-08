import Data.Sequence (Seq(Empty))
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
foldAT f z (Node nodo hijo1 hijo2 hijo3) = f nodo (foldAT f z hijo1) (foldAT f z  hijo1) (foldAT f z hijo1)



--3
unoxuno :: Procesador [a] [a] --que devuelve cada elemento de la lista en una lista singleton.
unoxuno = map(\x -> [x])

--E.g., unoxuno [3,1,4,1,5,9] ⇝ [[3],[1],[4],[1],[5],[9]]

sufijos :: Procesador [a] [a] --que devuelve todos los sufijos de la lista que se esta procesando.
sufijos [] = [[]]
sufijos (x:xs) = (x:xs) : sufijos xs
--E.g., usando la notaci´on de strings para [Char], sufijos "Plp" ⇝ ["Plp", "lp", "p", ""]

--4

preorder:: AT a -> [a]
preorder Nil= []
preorder (Node nodo hijo1 hijo2 hijo3) = [nodo] ++ preorder hijo1 ++  preorder hijo2 ++  preorder hijo3

postorder:: AT a -> [a]
postorder Nil= []
postorder (Node nodo hijo1 hijo2 hijo3) = postorder hijo1 ++  postorder hijo2 ++  postorder hijo3 ++ [nodo]

inorder:: AT a -> [a]
inorder Nil= []
inorder (Node nodo hijo1 hijo2 hijo3) = inorder hijo1 ++  inorder hijo2 ++  [nodo] ++ inorder hijo3 

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

--7

--8
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc condicion proc1 proc2 = \x -> if condicion x then proc1 x else proc2 x

(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) proc1 proc2 = \x -> proc1 x  ++ proc2 x

(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) proc1 proc2 = \x -> concatMap proc1 (proc2 x)
--((\z->[0..z]) .! (map (+1))) [1,3] -> [0,1,2,0,1,2,3,4]