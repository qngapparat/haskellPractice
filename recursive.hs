

max' :: (Ord a) => [a] -> a
max' [] = error "list is empty"
max' [x] = x --edge case
max' (x:xs) = x `max` (max' xs)

replicate' :: (Num i, Ord i) => i->a->[a]
replicate' n x
	| n <= 0 = []
	| otherwise = x:(replicate' (n-1) x)


take' :: (Num i, Ord i) => i->[a]->[a]
take' n _
	| n <= 0 = []
take' _ [] = error "empty/too short list"
take' n (x:xs) = x:take' (n-1) xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse (x:xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x


zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 

elem' :: (Eq a) => a->[a]->Bool
elem' _ [] = False
elem' a (x:xs)
	| a == x = True
	| otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []
quicksort (x:xs) =
	let smallerPart = quicksort [a | a <- xs, a <= x];
		biggerpart = quicksort [a | a <- xs, a > x]
	in smallerPart ++ [x] ++ biggerpart



multThree :: (Num a) => a->a->a -> a
multThree x y z = x*y*z


let multWithTwo = multThree 2
let multWithEight = multWithTwo 4
let multWithSixteen = multWithEight 2

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)