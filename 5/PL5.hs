data Num a => Node a = C a a
data Num a => Shape a = Circle (Node a) a | Triangle (Node a) (Node a) (Node a)

class Moveable f where
	move :: Num a => f a -> Node a -> f a
	scale :: Num a => f a -> a -> f a
	-- rotate :: f a -> f a
	
class Moveable f => Measurable f where
	area :: Num a => f a -> a
	circum :: Num a => f a -> a

instance Moveable Shape where
	move (Circle (C x y) r) (C x' y') = Circle (C (x+x') (y+y')) r
	scale (Circle c r) s = Circle c (r*s)
	
instance Measurable Shape where
	area (Circle _ r) = r*r*3
	circum (Circle _ r) = 2*r*3