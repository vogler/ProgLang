module Program where

   type Prog = ([Decl], [Expr])
   
   data Var          = X Int
   data TVar         = A Int

   data Value =   Nu Int                    -- number
                | Bo Bool                   -- boolean
                | Closure Var Expr ValueEnv -- closure
                | L [Value]                 -- lists
                | T [Value]                 -- tuple
  
   data Expr = N Int                        -- number
             | B Bool                       -- boolean 
             | V Var                        -- variable
             | Lambda Var Expr              -- \x.e
             | App    Expr Expr             -- application: e1 e2
             | Let    Var Expr Expr         -- let x = e1 in e2
             | Tuple  [Expr]                -- (,..,) tuple constructor
             | E                            -- empty list
             | Cons   Expr Expr             -- (:) list constructor
{--required for problem d) only --}
             | Case Expr [(Pattern,Expr)]   -- case expression  
             | Binop B Expr Expr            -- binary operator application  
           

   data  B = Add | Sub | Mul | Div

   data TExpr = TInt                        -- Int
             | TBool                        -- Bool
             | TV TVar                      -- alpha (type variable)
             | TFn TExpr TExpr              -- alpha -> beta
             | TTuple [TExpr]               -- (alpha,beta,gamma, ...)
             | TList TExpr TExpr            -- [alpha]
             | TAp TExpr TExpr              -- alpha alpha
             | TCon                         -- type constructor
             | Empty                        -- empty type expression 
      
   type Scheme  = ([TVar], TExpr)

   type TypeEnv = [(Var,Scheme)]
   
   type ValueEnv = [(Var, Value)]


{--required for problem d) only --}
   data Pattern =   PVar Var  -- matches any value;binds result to vars
                  | PWildcard -- wildcard pattern;(does not bind vars)
                  | PLit Int  -- matches only integer values
                  | PList [Pattern]
                  | PTuple [Pattern]
                  | PCon VCon [Pattern]  -- constructor pattern 


 {-- required for problem c) only --}
   data Decl =   GLet Pattern Expr
               | Data TCon [TVar] [(VCon, TExpr)]

 {--required for problem c) only --}
   data Kind = Star | K Kind Kind
   
   
   
  {--TODO: TCon for type constructors and 
    VCon for value constructors have to be implemented --}
   data VCon = G | Z | J
   data TCon = F | D | M
                 