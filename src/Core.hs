{-# LANGUAGE DeriveTraversable, DataKinds, TypeOperators #-}
module Core where

import Control.Comonad.Cofree
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad.Reader
import GHC.TypeLits

data Term v a = Var v
  | a :$ a
  | Con !Name [a]
  | Lit Literal
  | Lam v a
  deriving (Show, Eq, Functor, Foldable, Traversable)

type JavaScript = String

data Literal = LInt !Int | LDbl !Double | LStr !String
  | LJavaScript !JavaScript
  deriving (Show, Eq)

litToJS :: Literal -> JavaScript
litToJS (LInt x) = show x
litToJS (LDbl x) = show x
litToJS (LStr x) = show x
litToJS (LJavaScript x) = "(" ++ x ++ ")"

parens :: JavaScript -> JavaScript
parens x = "(" ++ x ++ ")"
{-
compile :: Cofree (Term Int) a -> JavaScript
compile (Var i) = "v" ++ show i
compile (Literal l) = litToJS l
compile (f :$ a) = compile n f ++ parens (compile n a)
compile (Lam e) = concat ["function(", "v" ++ show n, "){", compile (n + 1) e, "}"]
-}

type Expr v = Cofree (Term v)

data Typed n = Typed (Expr Int (Typed (n + 1)))
  | UT
  deriving Show

type Name = String

data TCState = TCState
  { tcVars :: !(IM.IntMap (Typed 0))
  , tcFresh :: !Int }

fresh :: TC Int
fresh = do
  tc <- get
  let i = tcFresh tc
  put $ tc { tcFresh = i + 1}
  return i

data TCEnv = TCEnv
    { varTypeBinding :: IM.IntMap Type
    , conDefinitions :: M.Map Name (Type, [Type])
    }

type TC = ReaderT TCEnv (StateT TCState (Either String))

typeError :: String -> TC a
typeError = lift . lift . Left

varIs :: Int -> Typed 0 -> TC ()
varIs i e = do
  tc <- get
  forM_ (IM.lookup i $ tcVars tc)
    $ \e' -> unifyTyped e e'
  put $ tc { tcVars = IM.insert i e $ tcVars tc }

unifyExpr :: Expr Int Kind -> Expr Int Kind -> TC ()
unifyExpr (_ :< s) (_ :< t) = unifyTerm s t

type Type = Typed 0

type Kind = Typed 1

unifyTerm :: Term Int (Expr Int Kind) -> Term Int (Expr Int Kind) -> TC ()
unifyTerm (Var a) b = do
  varIs a (Typed $ UT :< b)
unifyTerm (Lit a) (Lit b)
  | a == b = return ()
  | otherwise = typeError $ "Literal doesn't match: " ++ show a ++ show b
unifyTerm (a :$ b) (c :$ d) = do
  unifyExpr a c
  unifyExpr b d
unifyTerm (Con name xs) (Con name' ys)
  | name == name' = sequence_ $ zipWith unifyExpr xs ys
  | otherwise = typeError $ "Couldn't match " ++ name ++ " with " ++ name'
unifyTerm (Lam _ a) (Lam _ b) = unifyExpr a b
unifyTerm a b = typeError $ "Couldn't match " ++ show a ++ " with " ++ show b

runTC :: TC a -> a
runTC m = case evalStateT (runReaderT m $ TCEnv mempty mempty) (TCState mempty 0) of
  Right a -> a
  Left str -> error str

typeLit :: Literal -> TC (Typed 0)
typeLit (LInt _) = pure $ Typed $ UT :< Con "Int" []
typeLit (LDbl _) = pure $ Typed $ UT :< Con "Double" []
typeLit (LStr _) = pure $ Typed $ UT :< Con "String" []
typeLit (LJavaScript _) = do
  a <- fresh
  pure $ Typed $ UT :< Con "JavaScript" [UT :< Var a]

typeTerm :: Term Int (Expr Int a) -> TC Type
typeTerm (Var i) = ask >>= \m -> case IM.lookup i $ varTypeBinding m of
  Just t -> pure t
  Nothing -> typeError $ "Unbound variable: " ++ show i
typeTerm (Lit l) = typeLit l
typeTerm (a :$ b) = do
  tRes@(Typed tRes') <- freshVar
  tFun <- typeExpr a
  Typed tArg <- typeExpr b
  unifyTyped tFun $ Typed $ UT :< Con "->" [tArg, tRes']
  return tRes
typeTerm (Lam v a) = do
  t@(Typed tArg) <- freshVar
  Typed tBody <- local (\env -> env { varTypeBinding = IM.insert v t (varTypeBinding env) }) $ typeExpr a
  return $ Typed $ UT :< Con "->" [tArg, tBody]
typeTerm (Con name xs) = ask >>= \m -> case M.lookup name $ conDefinitions m of
  Nothing -> typeError $ "Missing con definition: " ++ name
  Just (t, ts) -> do
    ys <- traverse typeExpr xs
    sequence_ $ zipWith unifyTyped ts ys
    return t

freshVar :: TC Type
freshVar = do
  i <- fresh
  return $ Typed $ UT :< Var i

typeExpr :: Expr Int a -> TC Type
typeExpr (_ :< e) = typeTerm e

unifyTyped :: Typed 0 -> Typed 0 -> TC ()
unifyTyped (Typed (_ :< s)) (Typed (_ :< t)) = unifyTerm s t
unifyTyped _ _ = return ()

var :: Int -> Expr Int ()
var i = () :< Var i

litInt :: Int -> Expr Int ()
litInt i = () :< Lit (LInt i)

($$) :: Expr Int () -> Expr Int () -> Expr Int ()
a $$ b = () :< (a :$ b)

(\-) :: Int -> Expr Int () -> Expr Int ()
i \- e = () :< Lam i e

{-
unifyTerm (Lit a) (Lit b)
  | a == b = return ()
unifyTerm (Lam a) = do

  e <- typeExpr a
unifyTerm ()

typeExpr :: Term v a -> TC
typeExpr ()
-}
