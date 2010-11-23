import List

--3.1 infinite lists and comprehensions
powers = [ x^3 | x<- [1..] ]

pairs = [(x,y) | x<-[2..], y<-[1..], x `mod` 2 == 0, y `mod` 2 /= 0]

onesList = let ones = 1: ones in [ take n ones | n <-[0..], n `mod`2 == 0 ]

fibs = 1:1: (zipWith (+)) fibs (tail fibs)
--fib@( 1: tailfib) = 1 : 1 : [ x+y | (x,y) <- zip fib tailfib]

primes = let sieve (n:ns) = n:sieve [ m | m <- ns, m `mod` n /= 0 ] in sieve [2..]

pyth n = [(x,y,z) | x <- [1..n], y <-[1..n], z <- [1..n], x*x + y*y  == z ^2]

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [ x:ps | x <- xs, ps <-perms ( (\\) xs [x]) ]

cut :: [a] -> [([a],[a])]
cut [] = []
cut (x:xs) = [([x],xs) | not (null xs)] ++ [(x:y,z) | (y,z) <- cut xs]

pars :: [a] -> [[[a]]]
pars [] = [[]]
pars xs = [[xs]] ++ [y:zs | (y,z) <- cut xs, zs <- pars z]

-----------------------
firsts_ :: [(a,b)] -> [a]
firsts_ xs = [x | (x,_) <- xs]

factors_ :: Int -> [Int]
factors_ n = [x | x <- [1..n], n `mod` x == 0]

concat_ :: [[a]] -> [a]
concat_ xss = [ x | xs <- xss, x <- xs]

replicate_ :: Int -> a -> [a]
replicate_ n x = [ x | _ <- [1..n]]

length_ :: [a] -> Int
length_ xs = sum [1 | _ <- xs]

scalar_ :: [Int] -> [Int] -> Int
scalar_ xs ys = sum [x*y | (x,y) <- zip xs ys]
   

--3.4 arithmetic puzzle
---straight-forward solution
ops :: Num a => a -> [b] -> [[b]]
ops 0 opl = []
ops 1 opl = map (:[]) opl
ops n opl = [ o:l | o <- opl, l <- ops (n-1) opl ]

compute :: [(a -> a -> a,b)] -> [a] -> a
compute opl (e:elems) = foldl ( \r (f,x) -> (fst f) r x )  e (zip opl elems)

allres :: [(a -> a -> a,b)] -> [a] -> [(a,[(a -> a -> a,b)])]
allres opl [e]   = [(e,[])]
allres opl elems = [ (compute ol elems, ol) | ol <- ops (length elems - 1) opl]

match :: [a] -> [a] -> [(a -> a -> a,b)] -> (a -> a -> Bool) -> [([b],[b])]
match l1 l2 opl c = let (a1,a2) = (allres opl l1, allres opl l2)
                    in [ (map snd o1, map snd o2) | (r1,o1) <- a1, (r2,o2) <- a2, c r1 r2]

sampleops = [((+),'+'),((-),'-'),((*),'*'),((/),'/')]


-- match [1,2,3,4,5] [17,19] sampleops (==)
-- all operators equal precedence, left-associative
-- [("-*-+","-"),("*-+-","-"),("/-*/","-")]
-- 1 - 2 * 3 - 4 + 5 = 17 - 19; 1 * 2 - 3 + 4 - 5 = 17-19 ; 1 / 2 - 3 * 4 / 5 = 17-19


---list comprehension solution
--match :: [a] -> [a] -> [(a -> a -> a,b) ] -> (a->a-> Bool) -> [([b], [b]) ]
{-
match [] _ ops e = error "empty list"
match [n] _ ops e  = error "only one element"
match ns1 ns2 ops e = [(e1, e2) |
                (e1, v1) <- expr (reverse ns1),
                (e2, v2) <- expr (reverse ns2),
                ((e) v1 v2)==True            ]
    where
      expr :: [Integer] -> [ ( [Char], Integer)]
      expr [n] = [([], n)]
      expr ns@(x:xs) = nub [ (o2++[op_s]++o1, ((op) v2 v1)) | 
		(o1, v1) <- expr [x], 
                (o2, v2) <- expr xs, 
                (op,op_s) <- reverse opers     ]


opers = [((+),'+'), ((-),'-')] 
-}


----subset sum problem:
subsetsumops = [((+),1), ((\x y -> x),0)]
findsum l x = map fst (match (0:l) [x] subsetsumops (==))

-- findsum [267, 493, 869, 961, 1000, 1153, 1246, 1598, 1766, 1922] 5842
-- 869 + 961 + 1000 + 1246 + 1766 = 5842
