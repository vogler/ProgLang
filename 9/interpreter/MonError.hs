module MonError where

data E a = Error String | Ok a
         deriving Show

instance Monad E where
   return x = Ok x
   m >>= f  = case m of
               Error s -> Error s
               Ok x -> f x
   fail s = Error s

tick :: E ()
tick = return ()

printOut :: Show a => E a -> String
printOut (Error s) = "Error: "++ s
printOut (Ok x)    = "Ok: " ++ show x

type M a = E a
