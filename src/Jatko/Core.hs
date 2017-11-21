{-# LANGUAGE DeriveTraversable #-}
module Jatko.Core where

import Control.Monad
import qualified Data.Map.Strict as M

type Name = String

data Expr a = Var a
  | Expr a :$ Expr a
  | Con !Name [Expr a]
  | Lit !Literal
  | Lam !Name (Expr a)
  | Case [(Name, [Name], Expr a)]
  | Expr a ::: Expr a
  | Forall !Name (Expr a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Expr where
  (<*>) = ap

instance Monad Expr where
  return = Var
  Var a >>= k = k a
  f :$ g >>= k = (f >>= k) :$ (g >>= k)
  Con name xs >>= k = Con name $ map (>>=k) xs
  Lit l >>= _ = Lit l
  Lam v e >>= k = Lam v (e >>= k)
  Case cs >>= k = Case [(n, vs, x >>= k) | (n, vs, x) <- cs]
  e ::: t >>= k = (e >>= k) ::: (t >>= k)

type JavaScript = String

data Literal = LInt !Int | LDbl !Double | LStr !String
  | LJavaScript !JavaScript
  deriving (Show, Eq)

data Declarations = Declarations
  { decVars :: M.Map Name (Expr Name)
  , decRegistries :: M.Map Name [(Name, [Name], Expr Name)]
  , decConstructors :: M.Map Name ([Name], [Expr Name], Expr Name)
  }

emptyDeclarations :: Declarations
emptyDeclarations = Declarations mempty mempty mempty
