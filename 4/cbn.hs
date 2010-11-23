module MiniInterpreter where

import List

data Exp = Number Int
         | Add Exp Exp
         | Id Identifier
         | Lambda Identifier Exp
         | App Exp Exp
         deriving Show

data Val = N Int
         | Closure Identifier Exp Env
         | Thunk Exp Env
         deriving Show

type Env = [(Identifier, Val)]

type Identifier = String

eval (Number n)   env = N n
eval (Add e1 e2)  env = add' (eval e1 env) (eval e2 env)
eval (Id x)       env = lookUp x env
eval (Lambda x e) env = Closure x e env
eval (App e1 e2)  env = let Closure x body env' = force (eval e1 env)
                            arg  = Thunk e2 env
                        in eval body ((x,arg):env')

force (Thunk e env) = force (eval e env)
force v = v

add' e1 e2 = let (N n1, N n2) = (force e1, force e2)
             in N (n1+n2)

lookUp x env = let Just (_,y) = find ((==x) . fst) env
                in y

e1 = App (Lambda "x" (Id "z")) (Number 4)
e2 = App (Lambda "z" e1) (Id "y")
e3 = App (Lambda "y" e2) (Add (Id "x") (Id "x"))
e4 = App (Lambda "x" e3) (Add (Number 4) (Number 5))


{-
   e4 corresponds to
   =================
   let x = 4+5
   in let y = x+x
      in let z = y
         in let x=4
            in z
-} 