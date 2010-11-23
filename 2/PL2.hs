import Test.HUnit

-- 2.1
data BTree a = Node a (BTree a) (BTree a)
            | Leaf deriving Show

-- a)
depth Leaf = 0
depth (Node k a b) = let da = 1+depth a in
	let db = 1+depth b in
	if da > db then da else db
-- b)
to_list Leaf = []
to_list (Node k a b) = (to_list a)++k:(to_list b)
-- c)
zip2 [] [] = []
zip2 (x:xs) [] = x:xs
zip2 [] (x:xs) = x:xs
zip2 (x:xs) (y:ys) = x:y:(zip2 xs ys)
to_list_bf Leaf = []
to_list_bf (Node k a b) = k:(zip2 (to_list_bf a) (to_list_bf b))
-- data MyList a = E | C a (MyList a) deriving Show
-- d)
tree_map f Leaf = Leaf
tree_map f (Node k a b) = Node (f k) (tree_map f a) (tree_map f b)
-- e)
find _ Leaf = False
find x (Node k _ _) | x == k = True
find x (Node k a b) | x < k = find x a
find x (Node k a b) | x > k = find x b
-- f)
insert x Leaf = Node x Leaf Leaf
insert x (Node k a b)
	| x <= k = Node k (insert x a) b
	| x > k = Node k a (insert x b)
-- g)
isBTree Leaf = True
isBTree (Node _ Leaf Leaf) = True
isBTree (Node k (Node a _ _) Leaf) = k >= a
isBTree (Node k Leaf (Node a _ _)) = k < a
isBTree (Node k (ta@(Node a _ _)) (tb@(Node b _ _))) = a <= k && k < b && isBTree ta && isBTree tb
-- isBTree (Node k a b) = let Node x _ _ = a in
	-- let Node y _ _ = b in
	-- x <= k && k < y && isBTree a && isBTree b

-- 2.2
-- concatMap :: (a -> [b]) -> [a] -> [b]
-- listComp e e1 e2 g = let xs = zip e1 e2 in
	-- concatMap (\(a,b) -> if g a b then [e a b] else []) xs
listComp e e1 e2 g = concatMap (\i -> map (e i) (filter (g i) e2)) e1

-- 2.3 see InterpretiveProgrammer.hs
-- fib 0 = 1
-- fib 1 = 1
-- fib n = (fib (n-1))+(fib (n-2))
-- fib n = Fun (\(Num x) -> Fun(\(Num y) -> Num (x+y)))

-- 2.4
data Lit = Int deriving Show
data Expr = Lit Int| Plus Lit Lit
-- data BinOp = Plus Lit Lit | Minus Lit Lit

eval (Lit x) = x
eval (Plus a b) = (eval a) + (eval b)

-- tests
all_tests = "tests" ~: TestList [
	"true" ~: (False ~? "fail")
	]
	
-- main = do runTestTT all_tests