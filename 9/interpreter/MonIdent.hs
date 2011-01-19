module MonIdent where

newtype I a = I a deriving Show

instance Monad I where
  return x = I x
  (I x) >>= f = f x
  fail i = error "lookup failed"

tick :: I ()
tick = return ()

printOut :: Show a => I a -> String
printOut (I x)    = show x

type M a = I a  
