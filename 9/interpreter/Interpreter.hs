module Interpreter where

type Value = Int
type Variable = String
type Env = [(Variable, Value)]

data Exp = Var Variable
          | Const Value
          | Plus Exp Exp
          | Let Variable Exp Exp
          deriving Show

eval :: Exp -> Env -> Value
eval (Var v) e = myLookup v e
eval (Const c) e = c
eval (Plus a1 a2) e = add (eval a1 e) (eval a2 e) 
eval (Let v a1 a2) e = let x = eval a1 e
                       in eval a2 (update e v x)

add :: Value -> Value -> Value
add x y = x+y

update :: Env -> Variable -> Value -> Env
update e v x = (v,x):e

myLookup :: Variable -> Env -> Value
myLookup v' ((v,x):e) | v'==v  = x
                      | otherwise = myLookup v' e
myLookup v' _ = error v'                                    

---------------------test cases 
test :: Exp -> String
test a = show (eval a [])

term1 = (Plus (Const 3) (Const 2))
term2 = (Plus (Const 3) (Var "x"))
term3 = (Let "x" (Plus (Const 2) (Const 3)) (Plus (Var "x") (Var "x")))

