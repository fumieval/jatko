{-# LANGUAGE DataKinds, TypeOperators, DeriveFunctor, LambdaCase #-}
module Jatko.TypeCheck where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Free

import Jatko.Core

import Debug.Trace

data TCState = TCState
  { tcVars :: !(IM.IntMap Type)
  , tcFresh :: !Int }
  deriving Show

fresh :: TCBase Int
fresh = do
  tc <- get
  let i = tcFresh tc
  put $ tc { tcFresh = i + 1}
  return i

data Var = VarName !Name | TyVar !Int deriving (Show, Eq)

data TCEnv = TCEnv
  { varTypeBinding :: M.Map Name Type
  , tcDecs :: Declarations
  , tcStack :: [String]
  }

type TCBase = ReaderT TCEnv (StateT TCState (Either String))
type TC = FreeT (Unbound Type) TCBase

typeError :: String -> TC a
typeError e = do
  env <- ask
  lift . lift . lift $ Left $ unlines (e : tcStack env)

type Type = Expr Var

with :: String -> TC a -> TC a
with s = local (\tc -> tc {tcStack = s : tcStack tc})

varIs :: Var -> Type -> TC ()
varIs (TyVar i) e = do
  tc <- get
  forM_ (IM.lookup i $ tcVars tc)
    $ \e' -> unifyType e e'
  put $ tc { tcVars = IM.insert i e $ tcVars tc }
varIs (VarName name) t = do
  env <- ask
  case M.lookup name $ varTypeBinding env of
    Just t' -> unifyType t t'
    Nothing -> wrap $ Unbound name env $ unifyType t

apply :: Expr Var -> Expr Var -> Expr Var
apply (Lam v e) a = e >>= \v' -> if VarName v == v' then a else Var v'
apply (Con c xs) b = Con c (xs ++ [b])
apply (e ::: _) x = apply e x
apply a b = a :$ b

unifyType :: Type -> Type -> TC ()
unifyType s0 t0 = with ("unify: " ++ show s0 ++ " === " ++ show t0) $ go s0 t0 where
  go (Var a) (Var b) | a == b = return ()
  go b (Var a) = varIs a b
  go (Var a) b = varIs a b
  go (Lit a) (Lit b)
    | a == b = return ()
    | otherwise = typeError $ "Literal mismatch: " ++ show a ++ show b
  go (a :$ b) c = do
    let t = apply a b
    unifyType t c
  go a (b :$ c) = do
    let t = apply b c
    unifyType a t
  go c0@(Con name xs) c1@(Con name' ys)
    | name == name', length xs == length ys = sequence_
      $ zipWith unifyType xs ys
    | otherwise = typeError "Couldn't match"
  go (Lam _ _) (Lam _ _) = typeError "Can't unify two quantified types"
  go a b = typeError $ "Couldn't match " ++ show a ++ " with " ++ show b

runTC :: Declarations -> TCBase a -> IO a
runTC decs m = case evalStateT (runReaderT m $ TCEnv mempty decs []) (TCState mempty 0) of
  Right a -> return a
  Left str -> putStrLn str >> fail ""

typeLit :: Literal -> TC Type
typeLit (LInt _) = pure $ Con "Int" []
typeLit (LDbl _) = pure $ Con "Double" []
typeLit (LStr _) = pure $ Con "String" []
typeLit (LJavaScript _) = freshVar

data Unbound r a = Unbound !Name !TCEnv (r -> a) deriving Functor

unbound :: Name -> TC Type
unbound name = do
  env <- ask
  wrap $ Unbound name env pure

resolve :: Declarations -> TC a -> TCBase a
resolve decs m = runFreeT m >>= \case
  Pure a -> return a
  Free (Unbound name env cont) -> local (\tc -> env) $ case M.lookup name $ decVars decs of
    Just dec -> resolve decs $ typeExpr dec >>= cont
    Nothing -> case M.lookup name $ decRegistries decs of
      Just dec -> resolve decs $ typeExpr (Case dec) >>= cont
      Nothing -> resolve decs $ typeError $ "Not in scope: " ++ name

refine :: Type -> TC Type
refine ty = do
  tc <- get
  let go e = e >>= \case
        TyVar i -> case IM.lookup i $ tcVars tc of
          Just t -> go t
          Nothing -> Var $ TyVar i
        VarName name -> Var $ VarName name
  return $ go ty

typeExpr :: Expr Name -> TC Type
typeExpr expr = with ("infer: " ++ show expr) $ case expr of
  Var name -> do
    env <- ask
    case M.lookup name $ varTypeBinding env of
      Just t -> pure t
      Nothing -> unbound name
  Lit l -> typeLit l
  a :$ b -> do
    tRes <- freshVar
    tFun <- typeExpr a
    tArg <- typeExpr b
    unifyType tFun $ Con "->" [tArg, tRes]
    refine tRes
  Lam v a -> do
    tArg <- freshVar
    tBody <- local (\env -> env { varTypeBinding = M.insert v tArg (varTypeBinding env) }) $ typeExpr a
    return $ Con "->" [tArg, tBody]
  e ::: sig -> do
    t <- typeExpr e
    let t' = fmap VarName sig
    unifyType t t'
    return t'
  Forall v e -> do
    qv <- freshVar
    local (\env -> env { varTypeBinding = M.insert v qv (varTypeBinding env) })
      $ typeExpr e
  Case clauses -> do
    tArg <- freshVar
    tRes <- freshVar
    env <- ask
    forM_ clauses $ \(con, vs, c) -> case M.lookup con $ decConstructors $ tcDecs env of
      Just (qs, ts, t) | length vs == length ts -> do
        unifyType tArg $ fmap VarName t
        freshVars <- replicateM (length qs) freshVar -- these shouldn't appear in the result
        local (\tc -> tc { varTypeBinding = M.fromList (zip qs freshVars)
          `M.union` M.fromList (zip vs $ map (fmap VarName) ts)
          `M.union` varTypeBinding tc }) $ do
            typeExpr c >>= unifyType tRes
      Just _ -> typeError "constructor arguments mismatch"
      Nothing -> typeError "constructor not found"
    return $ Con "->" [tArg, tRes]
  Con name xs -> do
    env <- ask
    case M.lookup name $ decConstructors $ tcDecs env of
      Nothing -> typeError $ "Missing con definition: " ++ name
      Just (qs, ts, t) -> do
        qvars <- replicateM (length qs) freshVar
        local
          (\tc -> tc {varTypeBinding = M.fromList (zip qs qvars) `M.union` varTypeBinding tc})
          $ do
            ys <- traverse typeExpr xs
            sequence_ $ zipWith unifyType (map (fmap VarName) ts) ys
        return $ foldr (\a r -> Con "->" [fmap VarName a, r])
          (fmap VarName t)
          (drop (length xs) ts)

freshVar :: TC Type
freshVar = do
  i <- lift fresh
  return $ Var $ TyVar i
