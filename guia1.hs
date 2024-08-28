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
sumasParciales (x:xs) = foldr(\y rec -> y : rec) [x] [xs]