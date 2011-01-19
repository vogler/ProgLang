
data RegEx c = Zero |
             One | 
             C c |
             Times (RegEx c) (RegEx c)|
             Plus (RegEx c) (RegEx c) |
             Star (RegEx c) |
             Cap (RegEx c) (RegEx c)|
             Neg (RegEx c)                  
             deriving Eq

instance Show c => Show (RegEx c) where
    show Zero = "{}"
    show One = "epsilon"
    show (C c) = show c
    show (Times r1 r2) = show r1 ++ show r2
    show (Plus r1 r2) = show r1 ++ " + " ++ show r2
    show (Star r) = "(" ++ show r ++ ")*"
    show (Cap r1 r2) = show r1 ++ " u " ++ show r2
    show (Neg r) = "not " ++ show r


accept :: Eq a => RegEx a -> [a] -> Bool
accept r s = acc r s null


acc :: Eq a => RegEx a -> [a] -> ([a] -> Bool) -> Bool
acc Zero s k = False
acc One s k = k s
acc (C c) [] k = False
acc (C c) (x:xs) k = if c == x then k xs else False
acc (Plus r1 r2) s k = 
    (acc r1 s k) || (acc r2 s k )
acc (Times r1 r2) s k =
    acc r1 s (\s' -> acc r2 s' k)
acc r@(Star r1) s k =
    (k s) || (acc r1 s (\s' -> acc r s' k) )
acc (Cap r1 r2) s k = 
    (acc r1 s k) && (acc r2 s k )
acc (Neg r) s k = not (acc r s k)


--eps_free :: RegEx -> RegEx
eps_free Zero = Zero
eps_free One = Zero
eps_free (C c) = (C c)
eps_free (Plus r1 r2) = Plus (eps_free r1) ( eps_free r2)
eps_free (Times r1 r2) = Plus ( Plus (Times (nullable r1) (eps_free r2) ) (Times (eps_free r1) (nullable r2) )) (Times (eps_free r1) (eps_free r2) ) 
eps_free (Cap r1 r2) = Plus ( Plus (Cap (nullable r1) (eps_free r2) ) (Cap (eps_free r1) (nullable r2) )) (Cap (eps_free r1) (eps_free r2) ) 
eps_free (Star r) = Times (eps_free r) (Star (eps_free r))
eps_free (Neg r) = Neg (Plus One r)

plus r1 r2 = if r1 == One || r2 == One then 
                 One
             else
                 Zero
times r1 r2 = if r1 == Zero || r2 == Zero then 
                 Zero
              else
                 One

nullable One = One
nullable Zero = Zero
nullable (C c) = Zero
nullable (Plus r1 r2) =  plus (nullable r1) ( nullable r2)
nullable (Times r1 r2) = times (nullable r1) (nullable r2)
nullable (Star r) = One
nullable (Cap r1 r2) = times (nullable r1) (nullable r2)
nullable (Neg r) = if (nullable r) == Zero then 
                       One
                   else  --nullable r = One
                       Zero 


accept' :: Eq a => RegEx a -> [a] -> Bool
accept' r s = acc (Plus (nullable r) (eps_free r))  s null

-- square in CPS 
sq n k = 

--fibonacci in CPS:
fib :: Integral a => a -> (a -> b) -> b
fib 0 k = k 0
fib 1 k = k 1
fib n k = fib (n - 2) (\x -> fib (n - 1) (\y -> k(x + y)))

--map in CPS:
mapCPS :: (a -> b) -> [a] -> ([b] -> c) -> c
mapCPS f [] k = k []
mapCPS f (x:xs) k = mapCPS f xs (\y -> k ((f x) : y) )
-- map f (x:xs) = f x : map f xs 

--filter in CPS:
filterCPS :: (a -> Bool) -> [a] -> ([a] -> b) -> b
filterCPS f [] k = k []
filterCPS f (x:xs) k = filterCPS f xs (\y -> k (if (f x) then x:y else y) )
-- filter f (x:xs) = if f x then x : filter f xs else filter f xs
