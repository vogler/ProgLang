data M z x y = S (z x) (z y)
data T x y z = K (x -> y (z, x))
data U x = U (x (U x))

data Z = Y Int