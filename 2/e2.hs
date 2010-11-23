module BTree where 

data BTree a = Nil  | Node a (BTree a) (BTree a)
                      --	deriving (Eq,Show)

instance (Eq a) => Eq (BTree a) where 
    Nil == Nil = True
    (Node a1 l1 r1 ) == (Node a2 l2 r2)  = a1 == a2 && l1 == l2 && r1 == r2
    _ == _ = False                                                              

instance Show a => Show (BTree a) where
  show (Nil) = ""
  show (Node x l r) = show x ++ " " ++ show l ++ show r

size :: BTree a -> Int
size Nil                = 0
size (Node n t1 t2) = 1 + size t1 + size t2

depth :: BTree a -> Int
depth  Nil = 0
depth (Node n t1 t2) = 1 + max(depth t1)  (depth t2)

--------dfs 
to_list :: BTree a -> [a]
to_list Nil = []
to_list (Node n t1 t2) = [n] ++ (to_list t1)  ++ (to_list t2)

---------bfs
child :: BTree a -> [BTree a]
child (Node _ l r) = [l,r]
child  _ = []

keys :: [BTree a] -> [a]
keys [] = []
keys (Nil:xs) = keys xs
keys ((Node k _ _):xs) = k : keys xs

conc :: [[BTree a]] -> [a]
conc ([]:_) = []
conc (xs:ls) = (keys xs) ++ (conc ls)

bfs :: BTree a -> [a]
bfs Nil = []
bfs t = let nodes = [t] : [ cl | n <- nodes, let cl = concatMap child n ]
        in conc nodes

-----------------------------------------------------
tree_map :: (a -> b) -> BTree a -> BTree b
tree_map f Nil = Nil
tree_map f (Node n t1 t2) = Node (f n) (tree_map f t1) (tree_map f t2)


find :: Ord a =>  a -> BTree a -> Bool
find x Nil = False
find x (Node n t1 t2) = if (x==n) then True
			else if (x<n) then find x t1
			else find x t2

insert :: Ord a => a -> BTree a -> BTree a
insert val Nil = Node val Nil Nil
insert val t@(Node n t1 t2) = if  val == n then t
			      else if val > n then insert val t2
			      else insert val t1 

isBTree :: Ord a => BTree a -> Bool
isBTree = isSorted . to_list
   where  isSorted xs = null xs || and (zipWith (<) (init xs) (tail xs))


----------test trees
tree1 :: BTree Int
tree1 =
    (Node 23  (Node 13  (Node 5  Nil 
                                 (Node 6 Nil Nil))
                        (Node 16 Nil Nil))
              (Node 42  (Node 31 Nil Nil)
                        (Node 68 Nil Nil)))

tree2 :: BTree Int
tree2 =
    (Node 33  (Node 13  (Node 5  Nil 
                                 (Node 6 Nil Nil))
                        (Node 16 Nil Nil))
              (Node 42  (Node 31 Nil Nil)
                        (Node 68 Nil Nil)))



---EXPRESSION EVALUATION-----------------------------------------
type Var = Char

data Expr = Lit Int  |
	    Var Var  |
	    Add Expr Expr |
	    Sub Expr Expr |
	    Div Expr Expr |
	    Mul Expr Expr |
            Let Var Expr Expr
	    deriving (Show, Read)

to_string :: Expr -> String
to_string (Lit n ) = show n
to_string (Var v ) = show v
to_string (Add e1 e2) = "("++ to_string e1 ++ " + " ++ to_string e2 ++ ")"
to_string (Sub e1 e2) = "("++ to_string e1 ++ " - " ++ to_string e2 ++ ")"
to_string (Mul e1 e2) = "("++ to_string e1 ++ " * " ++ to_string e2 ++ ")"
to_string (Div e1 e2) = "("++ to_string e1 ++ " / " ++ to_string e2 ++ ")"

op_count :: Expr -> Int
op_count (Lit n ) = 0
op_count (Var v ) = 0
op_count (Add e1 e2) = 1+(op_count e1) + (op_count e2)
op_count (Sub e1 e2) = 1+(op_count e1) + (op_count e2)
op_count (Mul e1 e2) = 1+(op_count e1) + (op_count e2)
op_count (Div e1 e2) = 1+(op_count e1) + (op_count e2)

data Env = Rho(Var -> Int)

init_e :: Env
init_e = Rho (\v -> 0)

set :: Int -> Env
set n = Rho (\v -> n)

value :: Env -> Var -> Int
value (Rho rho) v = rho v

update :: Env -> Var -> Int -> Env
update (Rho rho) v n 
	= Rho (\w -> if v == w then n else rho w)


eval :: Env -> Expr -> Int
eval s e = case e of 
	(Lit i) -> i
	(Var v) -> value s v
	(Add e1 e2) -> (eval s e1) + (eval s e2)
	(Sub e1 e2) -> (eval s e1) - (eval s e2)
	(Mul e1 e2) -> (eval s e1) * (eval s e2)
	(Div e1 e2) -> (eval s e1) `div` (eval s e2)
        (Let v e1 e2)-> let es = eval s e1 
                              in eval (update s v es) e2
---------------------------------------------------------------
