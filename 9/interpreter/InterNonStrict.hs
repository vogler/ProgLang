module InterNonStrict where

import MonIdent
--import MonError
--import MonStateTrans

type Value = Int
type Variable = String
type Env = [(Variable, M Value)]

data Exp = Var Variable
          | Const Value
          | Plus Exp Exp
          | Let Variable Exp Exp
          deriving Show

eval :: Exp -> Env -> M Value
eval (Var v) e = myLookup v e
eval (Const c) e = return c
eval (Plus a1 a2) e = do
                       v1 <- eval a1 e  
                       v2 <- eval a2 e          
                       add v1 v2
eval (Let v a1 a2) e = let x = eval a1 e
                       in eval a2 (update e v x)

add :: Value -> Value -> M Value
add x y = do
           tick
           return (x+y) 

update :: Env -> Variable -> M Value -> Env
update e v x = (v,x):e

myLookup :: Variable -> Env -> M Value
myLookup v' ((v,x):e) | v'==v  = x
                     | otherwise = myLookup v' e
myLookup v' _ = fail v'

---------------------test cases 
test :: Exp -> String
test a = printOut (eval a [])

term1 = (Plus (Const 3) (Const 2))
term2 = (Plus (Const 3) (Var "x"))
term3 = (Let "x" (Plus (Const 2) (Const 3)) (Plus (Var "x") (Var "x")))

