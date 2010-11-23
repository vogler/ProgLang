
-- function definition
first (x,y) = x

-- wildcard
second (_, y) = y

hd (x:_) = x

-- Laziness
ones = 1 : ones

ev = 0 : [ x+1 | x <- od ]
od = [x+1 | x <- ev]

fibs@(1:fib) = 1 : 1:  [ x+y | (x,y) <- zip fibs fib]

data MyList a = E | C a (MyList a)
  deriving Show

instance (Eq a) => Eq (MyList a) where
   (==) E E = True
   (==) (C x y) (C xx yy) = x==xx && y == yy
   (==) _ _ = False