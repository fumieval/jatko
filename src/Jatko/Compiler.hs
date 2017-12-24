module Jatko.Compiler where

import Jatko.Core
import Data.List (intercalate)
import Data.Char

mangle :: String -> String
mangle = concatMap go where
  go c
    | isAlphaNum c = pure c
    | otherwise = "_" ++ show (ord c)

litToJS :: Literal -> JavaScript
litToJS (LInt x) = show x
litToJS (LDbl x) = show x
litToJS (LStr x) = show x
litToJS (LJavaScript x) = "(" ++ x ++ ")"

parens :: JavaScript -> JavaScript
parens x = "(" ++ x ++ ")"

compile :: Expr Name -> JavaScript
compile (Var v) = mangle v
compile (Lit l) = litToJS l
compile (Con name xs) = "[" ++ intercalate "," (show name : map compile xs) ++ "]"
compile (f :$ a) = compile f ++ parens (compile a)
compile (Lam v e) = concat ["function(", mangle v, "){return ", compile e, ";}"]
compile (Coerce e) = compile e
compile (Case cs) = concat
  [ "function(con){"
  , "var clauses = {};"
  , concat [concat
    ["clauses[", show name, "]="
    , "function(args){"
    , concat ["var " ++ v ++ " = con[" ++ show i ++ "];" | (i, v) <- zip [1 :: Int ..] vs]
    , "return ", compile r, "};"]
    | (name, vs, r) <- cs]
  , "return clauses[con[0]](con)"
  , "}()"
  ]
compile (e ::: _) = compile e
compile (Forall _ _) = error "unsupported"
