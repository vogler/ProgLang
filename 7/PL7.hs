-- 7.1
sq n k = k (n*n)
fib 0 k = k 0
fib 1 k = k 1
fib n k = fib (n-1) $ \a -> fib (n-2) $ \b -> k $ (+) a b
map_ f [] k = k []
map_ f (x:xs) k = map_ f xs $ \ys -> k $ (f x):ys
filter_ f [] k = k []
filter_ f (x:xs) k = filter_ f xs $ \ys -> k $ if (f x) then x:ys else ys