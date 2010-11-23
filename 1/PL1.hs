-- 1.1
len [] = 0
len l = 1 + len (tail l)

mean [] = undefined
mean l = (sum l) / (len l)

-- pal l = l ++ (reverse l)
pal [] = []
pal l = (head l) : pal (tail l) ++ [head l]

isPal [] = True
isPal l = (head l) == (last l) && isPal (tail (reverse (tail l)))

flatten [] _ = []
flatten [l] _ = l
flatten l s = (head l) ++ (s : (flatten (tail l) s))

-- 1.2
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldl :: (a -> b -> a) -> a -> [b] -> a
foldr_ f x [] = x
foldr_ f x xs = f (head xs) (foldr f x (tail xs))
foldl_ f x [] = x
foldl_ f x xs = foldl f (f x (head xs)) (tail xs)
sum_ [] = undefined
sum_ (x:xs) = foldr_ (\a b -> a+b) x xs
product_ [] = undefined
product_ (x:xs) = foldr_ (\a b -> a*b) x xs
and_ [] = undefined
and_ (x:xs) = foldr_ (\a b -> a && b) x xs
or_ [] = undefined
or_ (x:xs) = foldr_ (\a b -> a || b) x xs
reverse_ [] = []
reverse_ (x:xs) = foldl_ (\a b -> b:a) [x] xs
map_ f [] = []
map_ f (x:xs) = foldr_ (\a b -> (f a):b) [f x] xs
length_ [] = 0
length_ (x:xs) = foldl_ (\a b -> a+1) 1 xs
filter_ f [] = []
filter_ f l = foldr_ (\a b -> if f a then a:b else b) [] l
-- filter_ (\x -> mod x 2 == 0) [1..5]
-- foldl = foldr when: 

-- 1.3
find _ [] = False
find x [y] = x == y
find x l = (head l) == x || (find x (tail l))

union a [] = a
union a b = a ++ [x | x <- b, (find x a) == False]

intersect a b = [x | x <- a, (find x b)]

diff a b = [x | x <- (union a b), (find x (intersect a b)) == False]

-- 1.4
-- a)
insert x [] = [x]
insert x l = let y = (head l) in
	if x<y then x : l else y : (insert x (tail l))

isort [] = []
isort l = insert (head l) (isort (tail l))

-- b)
split [] = ([],[])
split (x:[]) = ([x],[])
split (x:y:xs) = let (a,b) = split xs in (x:a,y:b)
-- split l = (take ((len l)/2) l, take ((len l)/2) (reverse l))
merge a [] = a
merge [] a = a
merge (x:xs) (y:ys) = if x<y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)
merge_sort [] = []
merge_sort [a] = [a]
merge_sort l = let (a,b) = split l in merge (merge_sort a) (merge_sort b)

-- 1.5
level '(' = 1
level ')' = -1
level _ = 0
levels s = map level s
nestedSum s = (sum (levels s)) == 0
nestedScan s = scanl (+) 0 (levels s)
nested s = and (map (\x -> x >= 0) (nestedScan s)) && (nestedSum s)

-- 1.6

