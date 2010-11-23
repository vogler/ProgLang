import List

-- function definition
first (x,y) = x

-- wildcard
second (_, y) = y

hd (x:_) = x

-- Laziness
ones = 1 : ones

ev = 0 : [ x+1 | x <- od ]
od = [ x+1 | x <- ev ]


-- 3.1
-- a)
pw3 = [ 3^x | x <- [0..] ]
-- b)
odev = [ x | x <- zip od ev ]
-- c)
evenOnes = [ take x ones | x <- ev ]
-- d)
fibs@(1:fib) = 1 : 1:  [ x+y | (x,y) <- zip fibs fib ]
-- e)
primes = sieve [2..]
sieve (p : xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]
-- f)
pyth x = [ (a, b, c) | a <- [0..x], b <- [0..x], c <- [0..x], a*a+b*b==c*c ]
-- g)
--flatten = foldl (++) []
perms [] = [[]]
--perms xs = [ map ((:)x) (perms (delete x xs)) | x <- xs ]
perms xs = [ x:ys | x <- xs, ys <- perms (delete x xs) ]
-- h)
--munion a b = [ x | x <- a, elem x b == False ]
subsets [] = [[]]
subsets xs = []:[ a:b | a <- nub xs, let (_:tl) = dropWhile (/=a) xs, b <- subsets tl ]
_partition :: Eq a => [a] -> [[[a]]]
_partition xs = [xs]:[ x:ys | x <- (subsets xs) \\ [[],xs], ys <- _partition (xs \\ x) ]
multiPart [] = [[]]
multiPart xs = [ a:b | a <- takeWhile ((head xs ==) . head) $ tail $
 subsets xs, b <- multiPart $ xs \\ a, null b || a <= head b ]


-- a)
firsts xs = [ a | (a,b) <- xs]
-- b)
factors n = [ x | x <- [1..n], n `mod` x == 0]
-- c)
_concat xs = [ y | ys <- xs, y <- ys ]
-- d)
_replicate n x = [ x | _ <- [1..n] ]
-- e)
_length xs = last [ n | (n,x) <- zip [1..] xs ]
-- f)
scalar xs ys = sum [ x*y | (x,y) <- zip xs ys ]


-- 3.1
-- a)
--f (x:xs) (y:ys) = x+y : f xs ys
--f [1] [2]
-- d)
--[Int x | Int x <- [Int 1, Float 1.2, Int 2]]
-- g)
--f = \(~[x]) -> x
-- j)
f = \(~[x, (a,b)]) -> x

--evSum n = sum (take n ev)
--evSum = sum . ((flip take) ev)


-- 3.4)
sampleops = [((+),'+'),((-),'-'),((*),'*'),((/),'/')]
ops = map (\(a,b) -> b) sampleops
opParts n ops = nub $ map (\x -> take n x) $ perms $ concat $ map (\x -> replicate n x) ops