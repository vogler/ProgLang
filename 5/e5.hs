
-- Exercise 5.2
-- uncomment the following expression and try to infer its type

{-
   f = let a x = (x,x) in
       let b x = a $ a x in
       let c x = b $ b x in
       let d x = c $ c x in
       let e x = d $ d x in
       let f x = e $ e x in f (\x -> x)
-}


-- Exercise 5.3.
-- very extended 

--- shapes
module Shape(Vector, Vertex, Shape, Moveable, Measurable, dist) where 

data Floating a => Vector a = Vector a a
data Floating a => Vertex a = Vertex a a 
                        deriving Show
data Floating a => Shape a = Line (Vertex a) (Vertex a) 
                      | Circle (Vertex a) a 
                      | Triangle (Vertex a) (Vertex a) (Vertex a)
            deriving Show

class Moveable f  where
    move   :: (Floating a) => Vector a -> f a -> f a
    negX   :: (Floating a) => f a -> f a
    negY   :: (Floating a) => f a -> f a
    scale  :: (Floating a) => f a -> a -> f a
    rotate :: (Floating a) => f a -> f a
    rotate = negX . negY

class (Moveable f) => Measurable f where
    area :: (Floating a) => f a -> a
    perimeter :: (Floating a ) => f a -> a

--make vertex an instance of Eq
instance (Eq a, Floating a) => Eq (Vertex a) where 
    (Vertex x1 y1) == (Vertex x2 y2) = x1 == x2 && y1 == y2
    (Vertex x1 y1) /= (Vertex x2 y2) = if x1 /= x2 then True else y1 /= y2


--make vertex an instance of Movable 
instance Moveable Vertex where
    move  (Vector x1 y1) (Vertex x2 y2) = Vertex (x2+x1) (y2+y1)
    negX  (Vertex x y)    = Vertex x (-y)
    negY   (Vertex x y)   = Vertex (-x) y
    scale  (Vertex x y) f = Vertex (f * x) ( f * y)
    rotate (Vertex x y)   = Vertex (-x) (-y)

--make vertex an instance of Measurable 
instance Measurable Vertex where
    area  (Vertex x y) = 0
    perimeter (Vertex x y) = 0


dist (Vertex x1 y1) (Vertex x2 y2) = sqrt (( x2-x1) * (x2-x1) + (y2-y1) * (y2-y1) ) 
instance Measurable Shape where
    area (Circle x r) = pi * r * r
    area  (Triangle x y z) = sqrt (s* (s-a) * (s-b) * (s-c))
                             where
                               a = dist x y
                               b = dist x z
                               c = dist y z
                               s = (a+b+c) / 2
    area (Line x y) = 0
    perimeter (Triangle x y z) = (dist x y) + (dist x z) + (dist y z)
    perimeter (Circle x r) = 2 * pi *r
    perimeter (Line x y) = dist x y

instance Moveable Shape where
    move  d (Line x y) = Line (move d x) (move d y)
    move  d (Circle x r) = Circle (move d x) r
    negX  (Line x y)   = Line (negX x) (negX y)
    negX  (Circle x r)   = Circle (negX x) r
    negY  (Line x y)   = Line (negY x) (negY y)
    negY  (Circle x r)   = Circle (negY x) r
    scale (Line x y) f = Line (scale x f ) (scale y f)
    scale (Circle x r) f = Circle (scale x f) (r*f)

