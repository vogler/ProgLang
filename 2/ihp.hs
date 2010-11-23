--Interpretive Haskell Programmer
-- a dynamically-typed term language
import List

data Term = Occ Var
          | Use Prim
          | Lit Integer
          | App Term Term
          | Abs Var  Term
          | Rec Var  Term
          | Let Var Term Term

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
eval env (Let x e1 e2) = eval((x,e):env) e2 where  e = eval env e1
 

free (Occ x) = [x]
free (Use c) = []         
free (Lit k ) = []          
free (App m n ) = nub (free m ++ free n) 
free (Abs x m) =  (free m) \\ [x]
free (Rec x m ) =  (free m) \\ [x]
free (Let x e1 e2) = (nub (free e1 ++ free e2 )) \\ [x]


-- a (fixed) "environment" of language primitives
times = binOp Num  (*)
minus = binOp Num  (-)
plus  = binOp Num  (+)
equal = binOp Bool (==)
cond  = Fun (\b -> Fun (\x -> Fun (\y -> if (prjBool b) then x else y)))

prims = [ ("*", times), ("-", minus), ("+", plus), ("==", equal), ("if", cond) ]


-- a term representing factorial and a "wrapper" for evaluation
facTerm = Rec "f" (Abs "n" 
              (App (App (App (Use "if")
                   (App (App (Use "==") (Occ "n")) (Lit 0))) (Lit 1))
                   (App (App (Use "*")  (Occ "n"))
                        (App (Occ "f")  
                             (App (App (Use "-") (Occ "n")) (Lit 1))))))
          
fac n = prjNum (eval [] (App facTerm (Lit n)))          
          
-- Fibonacci nubmers
fibTerm = Rec "f" (Abs "n"
                   (App
                    (App (App (Use "if") (App (App (Use "==") (Occ "n")) (Lit 0))) (Lit 1))
                   (App
                    (App (App (Use "if") (App (App (Use "==") (Occ "n")) (Lit 1))) (Lit 1))
                    (App (App (Use "+")  (App (Occ "f") (App (App (Use "-") (Occ "n")) (Lit 1))))
                     (App (Occ "f") (App (App (Use "-") (Occ "n")) (Lit 2)))
                    )
                   )
                   )                  
                  )

fib n = prjNum (eval [] (App fibTerm (Lit n)))