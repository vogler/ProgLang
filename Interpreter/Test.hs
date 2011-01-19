module Test where

import Program

--------------examples for testing type checking
-- e1: \x -> x
e1 = Lambda (X 2) (V (X 2))

-- e2: let f = \x -> x in f f
e2 = Let (X 1) (Lambda (X 2) (V (X 2))) (App (V (X 1)) (V (X 1)))

-- e3: \f g h -> (f g) (g h) 
e3 = Lambda (X 1) (Lambda (X 2) (Lambda (X 3)
        (App (App (V (X 1)) (V (X 2)))  (App (V (X 2)) (V (X 3))) )))

-- e4: \f g h -> (f g) (f h) 
e4 = Lambda (X 1) (Lambda (X 2) (Lambda (X 3)
        (App (App (V (X 1)) (V (X 2)))  (App (V (X 1)) (V (X 3))) )))

-- e5: let f = \x -> [x] in f f  --[a -> [a]]
e5 =  Let (X 1) (Lambda (X 2) (Cons (V (X 2)) E)) (App (V (X 1)) (V (X 1)))

--------------examples for testing interpretation 

-- i1: let x = 1 in let y = 2 in x+y
i1 = Let (X 1) (N 1) (Let (X 2) (N 2) (Binop Add (V (X 1)) (V (X 2)) ) )

--i2: let x = \y -> y - 1 in 3
i2 = Let (X 1) (Lambda ( X 2) (Binop Sub (V (X 2)) (N 1) ) ) (N 3)

-- i3: let y = 2 in [y,(y+1), (y+2)]
i3 = Let (X 2) (N 2) (Cons ( V (X 2)) ((Cons (Binop Add (V (X 2)) (N 1) ) (Cons (Binop Add (V (X 2)) (N 2)  ) E))) )
    
-- i4: \x -> a  * b 
i4 = Lambda (X 1) (Binop Mul (V (X 3)) (V (X 4)) )     
  
--i5 =  let x = True in let y = \ z -> z in (x,y) 
i5 = Let (X 1) (B True) (Let (X 2)  (Lambda (X 5) (V (X 5))) (Tuple [(V (X 1)), (V (X 2))]) )

--i6 =  let y = \x -> [x] in let x = [1] in (y 2) ++ x  (scoping!)
i6 = Let (X 1) (Lambda (X 2) (Cons (V (X 2)) E))   
     (Let (X 2) (Cons (N 1) E) (Cons (App (V (X 1)) (N 2)) (V ( X 2))) )

------------------data type declarations
-- given the following typ   constructors : data TCon = F | D | M 
-- given the following value constructors : data VCon = G | Z | J
-- data M a = G | J a (kind * -> *)
d1 = Data M [(A 1)] [(G, Empty), (J, TV (A 1))]
-- hasKind d1  = K Star Star 

--data D a b = Z a b (kind * -> * -> *)
d2 = Data D [(A 2), (A 3)]  [ (Z, (TList (TV (A 2)) (TV (A 3))) ) ]
--hasKind d2 = K Star Star Star

--data F a = G | J a a (kind * -> *)
d3 = Data F [(A 1)] [(G, Empty), (J, (TList (TV (A 1)) (TV (A 1)) ) )]
--has Kind d3 = K Star Star

--- pattern matching

-- should only be implemented to support CASE expressions
-- therefore we provide case examples only, which should be typed
-- and evaluated

c1 = Lambda (X 1) (Case (V (X 1)) [(PLit 1, N 1),
                                   (PLit 2, N 1),
                                   (PVar (X 2), Binop Add (V (X 2)) (N 10) )])
c2 = Binop Sub (Binop Mul (App c1 (N 10)) (App c1 (N 0))) ((App c1 (N 2)))

c3 = Lambda (X 1) (Case (V (X 1)) [(PList [], E), (PList [PVar (X 1)], Cons (V (X 1)) (Cons (V (X 1)) E))])
c4 = App c3 (Cons (B True) E)