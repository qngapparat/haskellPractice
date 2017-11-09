import Data.List (isInfixOf)

--Problem: Once there is oid, isInfixOf does always succeed subsequently ~ no flushing


filter' :: String -> String 
filter' inp 
	| sample `isInfixOf` inp = inp
	| otherwise = ""
	where sample = "oid"

main = interact filter'
