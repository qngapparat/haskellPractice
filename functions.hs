divideByTen :: (Floating a) => a -> a 
divideByTen = (/10)

divideByTen 20



isUpper :: Char -> Bool
isUpper = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x  = f (f x )


zipWith' :: (a->b->c) -> a -> b -> c
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f x:xs y:ys = f x y : zipWith' f xs ys


flip' :: (a->b->c) -> (b->a->c)
flip' f x y  = f y x


map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs


filter' :: (a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) 
	| f x = x : filter' f xs --in case f x is True
	| otherwise = filter' f xs

 largestUnder100k :: (Integral a) -> a
 largestUnder100k = filter p [100000,99999..]
	where p x = x `mod` 3829 == 0


