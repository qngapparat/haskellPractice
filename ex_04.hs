data MyTree a = Null | MyNode a (MyTree a) (MyTree a)
	deriving (Eq,Show,Read)

collapse' :: Ord a => MyTree a -> [a]
collapse' Null = []
collapse' (MyNode value t1 t2) = collapse' t1 ++ value:[] ++ collapse' t2


isSorted :: Eq a => [a] -> Bool
isSorted [] = True
isSorted (x:xs) = (x >= head xs) && isSorted xs
