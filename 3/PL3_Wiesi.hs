import List

-- Problem 3.1

-- a)
powersOfThree = [3^x | x <- [0..]]
-- b)
oddAndEven = [(e, o) | e <- [2..], o <- [1..], e `mod` 2 == 0, o `mod` 2 /= 0]
-- c)
evenOnes = []:map ((1:).(1:)) evenOnes
-- d)
fibs = 0:1:zipWith (+) fibs (tail fibs)
-- e)
primes = sieve [2..] where
  sieve (x:xs) = x : filter ((/= 0).(`mod` x)) xs
-- f)
pyth n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2+b^2 == c^2]
-- g)
perms [] = [[]]
perms xs = [ x:ps | x <- xs, ps <- perms (xs \\ [x]) ]

-- a)
firsts xs = [a | (a, _) <- xs]
-- b)
factors n = [x | x <- [1..n], (n `mod` x) == 0]
-- c)
concat xs = [y | ys <- xs, y <- ys]
-- d)
replicate n a = [a | _ <- [1..n]]
-- e)
length xs = sum [1 | _ <- xs]
-- f)
scalar xs ys = sum [x*y | (x,y) <- zip xs ys]
