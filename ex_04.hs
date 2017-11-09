data MyTree a = Null | MyNode a (MyTree a) (MyTree a)
	deriving (Eq,Show,Read)

collapse' :: Ord a => MyTree a -> [a]
collapse' Null = []
collapse' (MyNode value t1 t2) = (collapse' t1) ++ value:[] ++ (collapse' t2)


---isListOrdered :: Ord a => [a] -> Bool
--isListOrdered [] = True
--isListOrdered (x:xs) = x <= (maximum xs) && isListOrdered xs

isListOrdered :: Ord a => [a] -> Bool
isListOrdered xs = all (\(x, y) -> x <= y) $ zip xs (tail xs)

isSorted :: Ord a => MyTree a -> Bool
isSorted t = isListOrdered $ collapse' t

--isSorted = collapse' . isListOrdered
-- or alternatively: 
-- isSorted t = collapse' . isListOrdered t
