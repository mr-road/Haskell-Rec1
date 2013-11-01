import Data.List

transformUserIDProductIDToUserProducts :: Eq a => [(a,b)] -> [(a,[b])]
transformUserIDProductIDToUserProducts tupleList =
	let uniqueAs inp = (nub $ fmap (fst) inp)
	    filter' inp = filter (\(f,s)-> f == inp)
	    findBs inp list = map (snd) (filter' inp list)
	in
		map (\uid -> (uid, findBs uid tupleList)) (uniqueAs tupleList)