data Expr a = Var a
  | Lit Literal
  | Expr a :$ Expr a
  | Lam (Expr a)
  | Expr a ::: Expr a
  | Let (Expr a) (Expr a)
  | Case (Expr a) (Expr b)

type JavaScript = String

data Literal = LInt !Int | LDbl !Double | LStr !String
  | LJavaScript !JavaScript

newtype Var = VarId !Int

litToJS :: Literal -> JavaScript
litToJS (LInt x) = show x
litToJS (LDbl x) = show x
litToJS (LStr x) = show x
litToJS (LJavaScript x) = "(" ++ x ++ ")"

parens :: JavaScript -> JavaScript
parens x = "(" ++ x ++ ")"

compile :: Int -> Expr Int -> JavaScript
compile _ (Var i) = "v" ++ show i
compile _ (Literal l) = litToJS l
compile n (f :$ a) = compile n f ++ parens (compile n a)
compile n (Lam e) = concat ["function(", "v" ++ show n, "){", compile (n + 1) e, "}"]
