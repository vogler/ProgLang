--4.3 b)
--id)
--IA:
fmap id (Node x []) = Node (id x) (map (fmap id) []) = Node x []
--IH:
fmap id (Node x xs) = Node (id x) (map (fmap id) xs) = Node x xs
--IS:
fmap id (Node l (x:xs)) = Node (id l) (map (fmap id) (x:xs)) = Node l (fmap id x):(map (fmap id) xs) = Node l x:xs


--f.g
--IA:
fmap f (fmap g (Node x [])) = fmap f (Node (g x) (map (fmap g) [])) = Node (f.g x) (map (fmap f) (map (fmap g) [])) = Node (f.g x) []
--IH:
fmap f (fmap g (Node x xs)) = fmap f (Node (g x) (map (fmap g) xs)) = Node (f.g x) (map (fmap f) (map (fmap g) xs)) = Node (f.g x) (f.g xs)
--IS:
fmap f (fmap g n) = fmap f (fmap g (Node l (x:xs))) = fmap f (Node (g l) (map (fmap g) (x:xs)))
 = Node (f.g l) (map (fmap f) (map (fmap g) x):(map (fmap g) xs))
 = Node (f.g l) (map (fmap f) (map (fmap g) x)):(map (fmap f) (map (fmap g) xs))
 = Node (f.g l) (f.g x):(map (fmap f) (map (fmap g) xs))
 = Node (f.g l) (f.g x):(f.g xs)
 = fmap (f.g) n