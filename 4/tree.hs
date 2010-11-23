
module TreeFun where

import List

data Tree a   = Node { rootLabel :: a,
                       subTrees  :: Forest a } 

type Forest a = [Tree a]

ex1 = Node 1 [Node 2 [], Node 3 [], Node 4 [Node 5 [], Node 6[], Node 7[]]]
ex2 = Node 1 [Node 2 [], Node 3 [], Node 4 [Node 5 [], Node 6[]]]
ex3 = Node True [Node False [Node True [], Node False []], Node True [Node True [], Node False []]]


instance Eq a => Eq (Tree a) where
  Node x t  == Node x' t'  =  (x==x') && 
                              length t == length t' && 
                              (all (==True) $ zipWith (==) t t')


instance Ord a => Ord (Tree a) where
 compare (Node x _) (Node y _) = compare x y 



instance Show a => Show (Tree a) where

   show = show' 0 where 
 
      show' n (Node x []) = indent n ++ show x ++ "\n"
      show' n (Node x t)  = let (l,r) = split t in 
              concatMap (show' (n + width x)) l ++
              indent n ++ show x ++ "\n" ++
              concatMap (show' (n + width x)) r 
      split x = (splitAt $ ((flip div $ 2) . length) x) x
      width = (+1) . length . show
      indent = flip take $ cycle " "



instance Functor Tree where
   fmap f t = Node ( (f . rootLabel) t )
              (map (fmap f) (subTrees t))

data Maybe_ a = Nothing_ | Just_ a
instance Functor Maybe_ where
	fmap f (Just_ a) = Just_ (f a)
	fmap _ Nothing_ = Nothing_

class Flatten m where
   flatten :: m a -> [a]

instance Flatten Tree where
   flatten t = map rootLabel $
               concat $
               takeWhile (not . null) $                
               iterate (concatMap subTrees) [t]

