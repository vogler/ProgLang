module MonStateTrans where

data ST s a = ST { unST::(s -> (a,s)) }

instance Monad (ST s) where
   return x  = ST (\s -> (x,s))
   m >>= f   = ST (\s -> let (x1,s1) = unST m s
                             (x2,s2) = unST (f x1) s1
                         in (x2,s2))
   fail x = error "lookup failed"
   
tick :: ST Int () 
tick = ST (\s -> ((),s+1))
        

printOut :: Show a => M a -> String
printOut m = case unST m 0 of
             (a,s) -> "Count: " ++ show s ++ ", Value: " ++ show a

type M a = ST Int a