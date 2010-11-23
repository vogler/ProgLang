
{-
  0. Just, Nothing, Left, (:), (,,), :kind, :t
  1. data Numbers, fruits, deriving (Eq, Show)
  2. data Peano, deriving (Eq, Show) : recursive
  3. parametric: Maybe, Either, matches
  4. recursive: myList, myTree, tmap, tfold
  5. factorial for Peano
-}


{-
  0. let
  1. list comprehensions, map, inorder
-}


data Tree a = Node a (Tree a) (Tree a)
            | Leaf

-- 3. factorial for Peano

data Peano = Zero | Succ Peano
--data MyList = Emp | Cons Int MyList
data MyList a =  Emp | Cons a (MyList a)

iter z s  Zero    = z
iter z s (Succ n) = s (iter z s n)

plus n = iter n     Succ
mult n = iter Zero (plus n)

fac  = snd . iter (one, one) (\(a,b) -> (Succ a, mult a b))

int n = iter 0 (1+) n

instance Show Peano where
  show = show . int

(zero : one : two : three : four : five : _) = iterate Succ Zero


fib = 1 : 1 : [ x+y | (x,y) <- zip fib (tail fib)]

-- Worksheet 2

l = let x = [[1,2],[3,4]] in [ x+y | x <- x, y <- x, x <- x]
f = \x -> x : z
_ : z = l
