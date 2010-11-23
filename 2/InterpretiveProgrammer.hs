-- set functions
find _ [] = False
find x [y] = x == y
find x l = (head l) == x || (find x (tail l))

union a [] = a
union a b = a ++ [x | x <- b, (find x a) == False]

intersect a b = [x | x <- a, (find x b)]

diff a b = [x | x <- (union a b), (find x (intersect a b)) == False]

munion a b = [x | x <- a, (find x b) == False]

-- a dynamically-typed term language

data Term = Occ Var
          | Use Prim
          | Lit Integer
          | App Term Term
          | Abs Var  Term
          | Rec Var  Term
		  | Let Var Term Term

vars (Occ v) = [v]
vars (App t1 t2) = union (vars t1) (vars t2)
vars (Abs v t) = munion (vars t) [v]
vars (Rec v t) = munion (vars t) [v]
vars (Let v t1 t2) = union (vars t1) (munion (vars t2) [v])
vars _ = []
-- fv (Occ v) = 1
-- fv (Abs v t) = (fv t)-1
-- fv (Rec v t) = (fv t)-1
-- fv (App t1 t2) = (fv t1) + (fv t2)
-- fv (Let v t1 t2) = (fv t1) + (fv (Abs v t2))
-- fv _ = 0
free_vars t = length (vars t)

type Var  = String
type Prim = String


-- a domain of values, including functions

data Value = Num  Integer
           | Bool Bool
           | Fun (Value -> Value)

instance Show Value where
  show (Num  n) = show n
  show (Bool b) = show b
  show (Fun  _) = ""

prjFun (Fun f) = f
prjFun  _      = error "bad function value"

prjNum (Num n) = n
prjNum  _      = error "bad numeric value"

prjBool (Bool b) = b
prjBool  _       = error "bad boolean value"

binOp inj f = Fun (\i -> (Fun (\j -> inj (f (prjNum i) (prjNum j)))))


-- environments mapping variables to values

type Env = [(Var, Value)]

getval x env =  case lookup x env of
                  Just v  -> v
                  Nothing -> error ("no value for " ++ x)


-- an environment-based evaluation function

eval env (Occ x) = getval x env
eval env (Use c) = getval c prims
eval env (Lit k) = Num k
eval env (App m n) = prjFun (eval env m) (eval env n)
eval env (Abs x m) = Fun  (\v -> eval ((x,v) : env) m)
eval env (Rec x m) = f where f = eval ((x,f) : env) m
eval env (Let v e1 e2) = let v2 = (eval env e1) in
	eval ((v,v2):env) e2


-- a (fixed) "environment" of language primitives

times = binOp Num  (*)
plus  = binOp Num  (+)
minus = binOp Num  (-)
equal = binOp Bool (==)
lt = binOp Bool (<)
cond  = Fun (\b -> Fun (\x -> Fun (\y -> if (prjBool b) then x else y)))

prims = [ ("*", times), ("+", plus), ("-", minus), ("==", equal), ("<", lt), ("if", cond) ]


-- a term representing factorial and a "wrapper" for evaluation

facTerm = Rec "f" (Abs "n" 
              (App (App (App (Use "if")
                   (App (App (Use "==") (Occ "n")) (Lit 0))) (Lit 1))
                   (App (App (Use "*")  (Occ "n"))
                        (App (Occ "f")  
                             (App (App (Use "-") (Occ "n")) (Lit 1))))))
							 
fac n = prjNum (eval [] (App facTerm (Lit n)))

-- b)
fibTerm = Rec "ff" (Abs "n" 
              (App (App (App (Use "if")
                   (App (App (Use "<") (Occ "n")) (Lit 2))) (Lit 1))
                   (App (App (Use "+") (App (Occ "ff")  
                             (App (App (Use "-") (Occ "n")) (Lit 1))))
                        (App (Occ "ff")  
                             (App (App (Use "-") (Occ "n")) (Lit 2))))))
fib n = prjNum (eval [] (App fibTerm (Lit n)))

freeTerm = Occ "x"
-- fv = prjNum (eval prims )