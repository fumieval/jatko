module Jatko.Pretty where

import Jatko.Core
import qualified Data.Map.Strict as M
import Text.Parser.Expression

parens :: Precedence -> Precedence -> String -> String
parens p q s
  | p >= q = "(" ++ s ++ ")"
  | otherwise = s

data Precedence = NoPrecedence | Precedence !Int | Application deriving (Eq, Ord)

ppLit :: Literal -> String
ppLit (LInt x) = show x
ppLit (LStr x) = show x
ppLit (LDbl x) = show x
ppLit (LJavaScript x) = "[|" ++ x ++ "|]"

ppExpr :: Declarations -> Precedence -> Expr Var -> String
ppExpr _ _ (Var (VarName a)) = a
ppExpr _ _ (Var (TyVar i)) = show i
ppExpr ds _ (a :$ b) = ppExpr ds Application a ++ " " ++ ppExpr ds Application b
ppExpr _ _ (Con name []) = name
ppExpr ds p (Con name xs) = case M.lookup name $ decOpTable ds of
  Just (assoc, q) | [l, r] <- xs -> parens p (Precedence q) $ unwords $ case assoc of
    AssocLeft -> [ppExpr ds (Precedence $ q + 1) l, name, ppExpr ds (Precedence q) r]
    AssocNone -> [ppExpr ds (Precedence q) l, name, ppExpr ds (Precedence q) r]
    AssocRight -> [ppExpr ds (Precedence q) l, name, ppExpr ds (Precedence $ q - 1) r]
  _ -> parens p Application $ name ++ concatMap ((' ' :) . ppExpr ds Application) xs
ppExpr _ _ (Lit l) = ppLit l
ppExpr ds _ (Lam v e) = "\\" ++ v ++ " -> " ++ ppExpr ds NoPrecedence e
ppExpr ds _ (Case clauses) = "\\case"
  ++ unwords [show con ++ unwords vs ++ " -> " ++ ppExpr ds NoPrecedence e | (con, vs, e) <- clauses]
ppExpr ds _ (a ::: b) = ppExpr ds NoPrecedence a ++ " : " ++ ppExpr ds NoPrecedence b
ppExpr ds _ (Forall v e) = "∀" ++ v ++ "." ++ ppExpr ds NoPrecedence e
ppExpr ds p (Coerce e) = ppExpr ds p e
