curry1 :: ((a,b)->c) -> a -> b-> c
curry1  f x y = f (x,y)

uncurry1 :: (a -> b-> c) -> ((a,b)->c)
uncurry1 f (x,y) = f x y

suma :: Num a => a -> a -> a
suma x y = x + y

elemento ::  Eq a =>  a -> [a] -> Bool
elemento _ [] = False
elemento x (y:ys) = x == y || elemento x ys 

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f [] = []
filter2 f (x:xs) = if f x then x:filter2 f xs else filter2 f xs 

map2 :: (a->b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x : map2 f xs

-- foldr2  (a -> b -> b) -> b -> [a] -> b
-- foldr2 f z [] = z
-- foldr2 f z (x:xs) = f x (foldr2 f z xs)

mapFold :: (a->b) -> [a] -> [b]
mapFold f (x:xs) = foldr(\x rec -> )