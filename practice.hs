bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
	| weight / (height ^ 2) <= 18.5 = "lol skeletal"
	| weight / (height ^ 2) <= 25 = "ok normal"
	| weight / (height ^ 2) <= 30 = "fatty fuck off"
	| otherwise = "naja die inneren werte zählen"
 
bmiTell' :: (RealFloat a) => a->a->String
bmiTell' weight height 
	| bmi <= skinny = "lol skeletal xd"
	| bmi <= normal = "ok normal" 
	| bmi <= fat = "lol fattie" 
	| otherwise = "fxcking obese dude"
	where 
		bmi = weight / height^2
		(skinny, normal, fat) = (18.0,25.0,30.0)

max' :: (Ord a) => a->a->a
max' a b
	| a < b = b
	| otherwise = a

compare' :: (Ord a) => a->a->Ordering
a `compare'` b
	| a < b = LT
	| a > b = GT
	| otherwise = EQ

initials :: String -> String -> String
initials firstName lastName = [f] ++ "." ++ [l] ++ "."
	where
		(f:_) = firstName
		(l:_) = lastName

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
	where
		bmi weight height = weight / height^2

calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w / h^2, bmi < 25]


-- head' :: [a] -> a  
-- head' xs = case xs of 
-- 	[] -> error "No head for empty lists!"  
--     (y:_) -> x 

--using cases
describeList :: [a] -> String
describeList xs = "The list " ++ case xs of
	[] -> "it's empty"
	[x] -> "singleton list"
	xs -> "some longer list"

--using local function definions
describeList' :: [a] -> String
describeList' xs = "The list is a " ++ what xs
	where
		what [] = "empty"
		what [x] = "singleton list"
		what xs = "normal list"



