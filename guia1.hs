curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

addPair :: (Int, Int) -> Int
addPair (x, y) = x + y

addCurried :: Int -> Int -> Int
addCurried = curry addPair
