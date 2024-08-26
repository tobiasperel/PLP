--preguntas
-- que seria f y como funciona bien
-- funciones anonimas

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f z [] = z -- aplicarle una funcion a una lista vacio me da el acumulardor
foldr2 f z (x:xs) = f x (foldr2 f z xs)

sum2 :: Int -> Int  -> Int
sum2 x y = foldr2 (+) 0 [x,y]

elem2 :: a -> [a] -> Bool
elem2 a [] = False
