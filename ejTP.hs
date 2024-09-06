type Procesador a b = a ->[b] --a es el tipo de dato de entrada y b el de salida

--1
procVacio :: Procesador a b --que para cualquier estructura devuelve un resultado vac´ıo.
procVacio _ = []

procId :: Procesador a a -- que devuelve un unico resultado, que contiene la estructura original completa.
procId x = [x]

procCola :: Procesador [a] a
procCola (x:xs) = xs


unoxuno :: Procesador [a] [a] --que devuelve cada elemento de la lista en una lista singleton.
unoxuno = map(\x -> [x])

--E.g., unoxuno [3,1,4,1,5,9] ⇝ [[3],[1],[4],[1],[5],[9]]

sufijos :: Procesador [a] [a] --que devuelve todos los sufijos de la lista que se esta procesando.
sufijos [] = [[]]
sufijos (x:xs) = (x:xs) : sufijos xs
--E.g., usando la notaci´on de strings para [Char], sufijos "Plp" ⇝ ["Plp", "lp", "p", ""]

