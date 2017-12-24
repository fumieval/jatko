{-# LANGUAGE DeriveTraversable, LambdaCase #-}
module Jatko.Core where

import Control.Monad
import qualified Data.Map.Strict as M
import Text.Parser.Expression (Assoc)

type Name = String

data Expr a = Var a
  | Expr a :$ Expr a
  | Con !Name [Expr a]
  | Lit !Literal
  | Lam !Name (Expr a)
  | Case [(Name, [Name], Expr a)]
  | Expr a ::: Expr a
  | Forall !Name (Expr a)
  | Coerce (Expr a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Expr where
  pure = Var
  (<*>) = ap

instance Monad Expr where
  return = Var
  Var a >>= k = k a
  f :$ g >>= k = (f >>= k) :$ (g >>= k)
  Con name xs >>= k = Con name $ map (>>=k) xs
  Lit l >>= _ = Lit l
  Lam v e >>= k = Lam v (e >>= k)
  Case cs >>= k = Case [(n, vs, x >>= k) | (n, vs, x) <- cs]
  Forall v e >>= k = Forall v (e >>= k)
  Coerce a >>= k = Coerce (a >>= k)
  e ::: t >>= k = (e >>= k) ::: (t >>= k)

type JavaScript = String

data Literal = LInt !Int | LDbl !Double | LStr !String
  | LJavaScript !JavaScript
  deriving (Show, Eq)

data Declarations = Declarations
  { decVars :: M.Map Name (Expr Name)
  , decRegistries :: M.Map Name [(Name, [Name], Expr Name)]
  , decConstructors :: M.Map Name ([Name], [Expr Name], Expr Name)
  , decOpTable :: M.Map Name (Assoc, Int)
  }

subst :: Eq a => a -> Expr a -> Expr a -> Expr a
subst v t e = e >>= \case
  x | v == x -> t
    | otherwise -> Var x

simplify :: Int -> Declarations -> Expr Name -> Expr Name
simplify 0 _ x = x
simplify phase decs expr = case expr of
  Var v
    | Just cs <- M.lookup v (decRegistries decs) -> simplify phase decs $ Case cs
    | Just d <- M.lookup v (decVars decs) -> simplify phase decs d
  Con con xs :$ x -> simplify phase decs $ Con con (xs ++ [x])
  Con _ [x] -> Coerce $ simplify phase decs x
  Con con xs -> Con con $ map (simplify phase decs) xs
  Case cs :$ Con con xs
    | [e] <- [foldr (uncurry subst) e $ zip vs xs | (con', vs, e) <- cs, con == con']
    -> simplify phase decs e
  Lam v e -> Lam v $ simplify phase decs e
  Lam v e :$ x -> simplify phase decs $ subst v x e
  a :$ Coerce b -> simplify (phase - 1) decs
    $ simplify phase decs a :$ simplify phase decs b
  Coerce a :$ b -> simplify (phase - 1) decs
    $ simplify phase decs a :$ simplify phase decs b
  a :$ b -> simplify (phase - 1) decs
    $ simplify phase decs a :$ simplify phase decs b
  Case [(_, [v], e)] -> Lam v $ simplify phase decs e
  Case clauses -> Case [(con, vs, simplify phase decs e) | (con, vs, e) <- clauses]
  a ::: b -> simplify phase decs a ::: simplify phase decs b
  Forall v a -> Forall v (simplify phase decs a)
  Coerce e -> Coerce $ simplify phase decs e
  Var x -> Var x
  Lit l -> Lit l


data Var = VarName !Name | TyVar !Int deriving (Show, Eq)

emptyDeclarations :: Declarations
emptyDeclarations = Declarations mempty mempty mempty mempty
