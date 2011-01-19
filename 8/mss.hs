module Mss where

import List

mss' = maximum . scanr (ope) 0  where
     ope x y = max (x+y) 0

x +-+ (acc, res) = let acc' = max 0 (x+acc)
                   in (acc', max res acc')
mss =  snd . foldr (+-+) (0,0)
