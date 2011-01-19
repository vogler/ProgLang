module RegularExpression where

data RegEx c = Zero |
             One | 
             Letter c |
             Times (RegEx c) (RegEx c)|
             Plus (RegEx c) (RegEx c) |
             Star (RegEx c) 
             deriving Eq

instance Show c => Show (RegEx c) where
    show Zero = "{}"
    show One = "epsilon"
    show (Letter c) = show c
    show (Times r1 r2) = show r1 ++ show r2
    show (Plus r1 r2) = show r1 ++ " + " ++ show r2
    show (Star r) = "(" ++ show r ++ ")*"

ex1 = Star (Plus (Letter 0)
                 (Times (Letter 1)
                        (Times (Star (Times (Letter 0)
                                     (Times (Star (Letter 1))
                                     (Letter 0))))
                        (Letter 1))))

s1 = [1,0,1,0,1,0,0,1,1,0,1,0,1,1,1,0,0,1,0,0,1,0,1,0]
s2 = [1,0,1,0,1,0,0,1,1,0,1,0,1,1,0,1,0,0,1,0,0,1,0,1,0]

-- ex1 = (0 + 1(01*0)*1)* 
-- s1  = 101010011010111001001010 \in L(r)?
-- s2  = 1010100110101101001001010 \in L(r)?


accept :: Eq a => RegEx a -> [a] -> Bool
accept r s = acc r s null

acc :: Eq a => RegEx a -> [a] -> ([a] -> Bool) -> Bool
acc Zero s k = False
acc One s k = k s
acc (Letter _) [] k = False
acc (Letter x) (y:ys) k = x==y && k ys
acc (Times r1 r2) s k = acc r1 s (\xs -> acc r2 xs k)
acc (Plus r1 r2) s k = acc r1 s k || acc r2 s k
acc (r@(Star r1)) s k = (k s) || (acc r1 s (\xs -> acc r xs k))
