module Interpreter(typeCheck, interpret, hasKind, module Program) where

   import Program

   type Error = String

   typeCheck     :: TypeEnv -> Expr -> Either Error Scheme
   interpret     :: ValueEnv -> Expr -> Value
   hasKind       :: TExpr -> Kind
 
   {---  TODO: your implementation  ---}
  
