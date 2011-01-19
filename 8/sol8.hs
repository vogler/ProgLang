
-- exercise 8.2 d and e

module Lambda where

import List

data Exp = V String
         | Exp :@: Exp
         | L String Exp
          deriving (Eq, Show)

fv (V x) = [x]
fv (f :@: x) = union (fv f) (fv x)
fv (L x e) = fv e \\ [x]

bv (V _) = []
bv (f :@: e) = union (bv f) (bv e)
bv (L x e) = union [x] (bv e)

subst x a = s where
   s (V w) | x==w = a
           | x/=w = V w
   s (f :@: x) = s f :@: s x
   s (L w exp)
      | x==w = L w exp
      | x/=w = let fresh = concat $ [x,w] ++ fv exp ++ fv a
               in L fresh (s (subst w (V fresh) exp))

alpha (V x) (V y) = x==y
alpha (f :@: x) (f' :@: x')  = alpha f f' && alpha x x'
alpha (L w exp) (L w' exp')  = alpha (subst w (V (w++w')) exp)
                                     (subst w' (V (w++w')) exp')
alpha _ _ = False

id1 = L "x" (V "x")
id2 = L "y" (V "y")
y = L "f" ((L "x" ((V "f") :@: ((V "x") :@: (V "x"))) :@: ((L "x" ((V "f") :@: ((V "x") :@: (V "x")))))))
y'  = L "f" ((L "y" ((V "f") :@: ((V "y") :@: (V "y"))) :@: ((L "x" ((V "f") :@: ((V "x") :@: (V "x")))))))
y'' = L "x" ((L "y" ((V "x") :@: ((V "y") :@: (V "y"))) :@: ((L "x" ((V "x") :@: ((V "x") :@: (V "x")))))))


---

-- number n: n fold application of f to x
-- const: the K operator :: a -> b -> a

number n f x = foldr (const f) x [1..n]

-- add n m = \n m f x. n f (m f x)
add n m = number n succ (number m succ 0)

-- multiply n m = \n m f x. n (m f) x
mult n m = number n (number m succ) 0

-- expo n m = \n m f x. m n f x
expo n m = (number m) (number n) succ 0

-- subtra n m = \n m f x. m pred (n f x)
subtra n m = number m pred (number n succ 0)