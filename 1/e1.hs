------list operations
--length:
l :: [a] -> Int
l []     = 0
l (_:xs) = 1 + l xs

-- mean 
s :: [Double] -> Double
s []     = 0
s (x:xs) = x + s xs

mean :: [Double] -> Double
mean xs = (s xs) / fromIntegral (length xs)

-- palindrome
p :: [a] -> [a]
p []     = []
p (x:xs) = [x] ++ p xs ++ [x]

isP :: Eq a => [a] -> Bool
isP xs = (xs == reverse xs) && (even (length xs))

---list flatten
flatten :: a -> [[a]] -> [a]
flatten _ []  = []
flatten _ [x] = x
flatten separator (x:xs) = x ++ [separator] ++ flatten separator xs


---Fold Operations
-- instances of foldr
foldrr :: (a -> b -> b) -> b -> [a] -> b
foldrr f v []     = v
foldrr f v (x:xs) = f x (foldrr f v xs) 

sum_r  = foldrr (+) 0
prod_r = foldrr (*) 1
and_r  = foldrr (&&) True
or_r   = foldrr (||) False

length_r   = foldrr (\ _ n -> n+1) 0
reverse_r  = foldrr (\x xs -> xs ++ [x] ) [] 
map_r f    = foldrr (\x xs -> f x : xs ) []
filter_r p = foldrr (\x xs -> if p x then x:xs else xs) []


-- instances of foldl
foldll :: (b -> a -> b) -> b -> [a]-> b
foldll f v [] = v
foldll f v (x:xs) = foldll f (f v x) xs

sum_l  = foldll (+) 0
prod_l = foldll (*) 1
and_l  = foldll (&&) True
or_l   = foldll (||) False

length_l   = foldll (\n _ -> n+1) 0
reverse_l  = foldll (\xs x -> x:xs) []
map_l f    = foldll (\xs x -> xs ++ [f x]) []
filter_l p = foldll (\xs x -> if p x then xs ++ [x] else xs ) []

--foldl (op) e xs = foldr (op) e xs
--if op is associative and e is a unit for op

--foldl f v xs = fold (\x g -> (\a -> g (f a x))) id xs v
sumlength :: [Int] -> (Int,Int)
sumlength xs = (sum xs, length xs)
sumLength = foldrr (\n (x,y) -> (n+x, 1+y)) (0,0)

-------------------------------sets as lists
find :: Eq a => a -> [a] -> Bool
find e l = 
  case l of
    []  -> False
    h:r -> if e==h then True
            else find e r

union :: Eq a => [a] -> [a] -> [a]
union l1 l2 = 
  case l1 of 
    []  -> l2
    h:r -> if find h l2 then union r l2
            else h:(union r l2)

intersect :: Eq a => [a] -> [a] -> [a]
intersect l1 l2 =
  case l1 of 
    []  -> []
    h:r -> if find h l2 then h:(intersect r l2)
            else intersect r l2

diff :: Eq a => [a] -> [a] -> [a]
diff l1 l2 =
  case l1 of 
    []  -> []
    h:r -> if find h l2 then diff r l2
            else h:(diff r l2)

------------quick sort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort s ++ [x] ++ qsort l
		where	
		s =[a | a <- xs, a<=x]
		l = [a | a <- xs, a>x]

-------insertion sort
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y   =  x:y:ys
		| otherwise =  y: insert x ys 

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

--------------merge sort
split :: [a] -> ([a],[a])
split = split2 ([],[]) where
	split2 (l1,l2) [] = (l1,l2)
	split2 (l1,l2) [x] = (x:l1,l2)
	split2 (l1,l2) (x:y:xs) = split2 (x:l1,y:l2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = let (a,b) = split l in merge (msort a) (msort b) where
	     merge :: (Ord a) => [a] -> [a] -> [a]
	     merge xs [] = xs
	     merge [] ys = ys
	     merge xxs@(x:xs) yys@(y:ys) 
        	 | x <= y = x : merge xs yys
	         | otherwise = y : merge xxs ys



-- correct nesting
represent :: [Char] -> [Int]
represent [] = []
represent (x:xs) = 
    if (x == '(') then 1:(represent xs)
    else if (x == ')') then (-1) : (represent xs)
    else 0 : (represent xs)



check :: [Char] -> Bool
check l = let l0 = represent l in 
           let l1 = scanr1 (+) l0 in 
            let l2 = map (<0) l1 in
             (head l1 == 0) && (foldr (&&) True l2)

--------n queens
n_queens :: Int -> [[(Int,Int)]]
n_queens n = foldr queen [[]] [1..n]
    where queen :: Int -> [[(Int,Int)]] -> [[(Int,Int)]]
          queen j qs = [ ((i,j):q) | q <- qs, i <- [1..n], all (safe (i,j)) q ]
              where safe :: (Int,Int) -> (Int,Int) -> Bool
                    safe (i,j) (k,l) = (i /= k) && (j /= l) && (abs (i-k) /= abs (j-l))
