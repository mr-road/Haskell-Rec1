transform :: [(a,b)] -> [(a,[b])]
transform x =
	let uniqueAs = nub $ fmap (fst) x
		findBs inp list = map (snd . (filter (\(f,s)-> f == inp))) list
	in  
		map (\(l,r) -> (l, findBs l x)) (uniqueAs x)