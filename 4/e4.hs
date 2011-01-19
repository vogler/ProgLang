module Set where 
import List 

--Problem 4.2 (Data Abstraction Set)
newtype Set a = SetC[a]

class S f where
    empty     :: (Ord a) => f a   
    isEmpty   :: (Ord a) => f a -> Bool
    singleton :: (Ord a) => a -> f a
    member    :: (Ord a) => f a -> a -> Bool
    insert    :: (Ord a) => f a -> a -> f a
    delete    :: (Ord a) => f a -> a -> f a
    diff      :: (Ord a) => f a -> f a -> f a

instance Eq a => Eq (Set a) where
    (SetC xs) == (SetC ys) = xs == ys


instance Ord a => Ord (Set a) where
    (SetC xs) <= (SetC ys) = subSet xs ys where 
                        subSet :: Ord a => [a] -> [a] -> Bool
                        subSet [] ys = True
                        subSet xs [] = False
                        subSet (x:xs) (y:ys) 
                               | x<y  = False
                               | x==y = subSet xs ys
                               | x>y  = subSet (x:xs) ys

instance Show a => Show (Set a) where
  show (SetC xs) = "{"++ concat (intersperse ", " (map show xs))++ "}"

instance S Set where 
 empty  = SetC []

 isEmpty (SetC xs) = null xs

 singleton x = SetC [x]

 member (SetC xs) x = mem xs x where
  mem [] _     = False
  mem (y:ys) x = x== y || x > y && mem ys x


 insert (SetC xs) y = SetC (ins xs y) where
    ins []       y = [y]
    ins l@(x:xs) y
      | x < y = x : ins xs y
      | x > y = y : l
      | otherwise = l


 delete (SetC xs) y = SetC ((\\) xs [y])

 diff (SetC xs) (SetC ys) = SetC (dif xs ys) 
  where dif    :: Ord a => [a] -> [a] -> [a]
        dif [] ys = []
        dif xs [] = xs
        dif (x:xs) (y:ys)  
          | x<y 	= x : dif xs (y:ys)
          | x==y 	= dif xs ys
          | otherwise 	= dif (x:xs) ys


--Problem 4.3 (Functor) 
class MyFunctor f where
  my_fmap :: (a-> b) -> f a -> f b  
  
data Tree a = Leaf a | Node (Tree a) (Tree a)

instance MyFunctor Tree where
  my_fmap f (Leaf a) = Leaf (f a)
  my_fmap f (Node l r ) = Node (my_fmap f l) (my_fmap f r)


instance MyFunctor Maybe where
   my_fmap f (Just x) = Just (f x)
   my_fmap _ Nothing  = Nothing
  
{- 
assumption:
my_fmap id (Leaf a) = Leaf (id a) = Leaf a

my_fmap f (my_fmap g (Leaf a)) = my_fmap f (Leaf (g a)) = Leaf (f (g (a))) = Leaf (f . g) a

inductive step:
my_fmap id (Node l r ) = Node (my_fmap id l) (my_fmap id r) = Node l r

my_fmap f (my_fmap g (Node l r )) = my_fmap f (Node (my_fmap g l) (my_fmap g r)) =
  Node (my_fmap f (my_fmap g l)) (my_famp f (my_fmap g r)) =
  Node ( my_fmap f (g l)) (my_fmap f (g r)) =
  Node (my_fmap (f.g) l ) (my_fmap (f.g) r)

-}
