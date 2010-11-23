--4.1


--4.2
class (Show a, Eq a) => Set a where
	empty::a
	is_empty::a -> Bool
	insert::(Eq b) => b -> a -> a
	-- delete::a -> a -> a
	--show::Set a -> String

instance (Show a, Eq a) => Set [a] where
	empty = []
	is_empty a = a == []
	insert x [] = [x]
	insert x a = x:a

--4.3
--a) tree.hs
--b) 4_3b.hs