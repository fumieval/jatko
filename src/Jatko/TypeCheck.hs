{-# LANGUAGE DataKinds, TypeOperators, DeriveFunctor, LambdaCase #-}
module Jatko.TypeCheck where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Jatko.Pretty

import Jatko.Core

data TCState = TCState
  { tcVars :: !(IM.IntMap Type)
  , tcFresh :: !Int
  }
  deriving Show

fresh :: TC Int
fresh = do
  tc <- get
  let i = tcFresh tc
  put $ tc { tcFresh = i + 1 }
  return i

data TCEnv = TCEnv
  { varTypeBinding :: M.Map Name Type
  , tcDecs :: Declarations
  , tcStack :: [String]
  }

type TC = ReaderT TCEnv (StateT TCState (Either String))

typeError :: String -> TC a
typeError e = do
  env <- ask
  lift . lift $ Left $ unlines (e : tcStack env)


type Type = Expr Var

with :: ((Expr Var -> String) -> String) -> TC a -> TC a
with s m = do
  decs <- asks tcDecs
  local (\tc -> tc {tcStack = s (ppExpr decs NoPrecedence) : tcStack tc}) $ m

varIs :: Var -> Type -> TC ()
varIs (TyVar i) e = with (\pp -> show i ++ " <- " ++ pp e) $ do
  tc <- get
  forM_ (IM.lookup i $ tcVars tc)
    $ \e' -> unifyType e e'
  put $ tc { tcVars = IM.insert i e $ tcVars tc }
varIs (VarName name) t = do
  env <- ask
  case M.lookup name $ varTypeBinding env of
    Just t' -> unifyType t t'
    Nothing -> unbound name >>= unifyType t

apply :: Expr Var -> Expr Var -> Expr Var
apply (Lam v e) a = e >>= \v' -> if VarName v == v' then a else Var v'
apply (Con c xs) b = Con c (xs ++ [b])
apply (e ::: _) x = apply e x
apply a b = a :$ b

unifyType :: Type -> Type -> TC ()
unifyType s0 t0 = with (\pp -> "unify: " ++ pp s0 ++ " === " ++ pp t0) $ go s0 t0 where
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
  go (Con name xs) (Con name' ys)
    | name == name', length xs == length ys = sequence_
      $ zipWith unifyType xs ys
    | otherwise = typeError "Couldn't match"
  go (Forall v e) t = withVar $ \q -> go t (e >>= \case
    VarName v' | v == v' -> q
    x -> Var x)
  go t (Forall v e) = withVar $ \q -> go t (e >>= \case
    VarName v' | v == v' -> q
    x -> Var x)
  go (Lam _ _) (Lam _ _) = typeError "Can't unify two quantified types"
  go a b = typeError $ "Couldn't match " ++ show a ++ " with " ++ show b

runTC :: Declarations -> TC a -> IO a
runTC decs m = case evalStateT (runReaderT m $ TCEnv mempty decs []) (TCState mempty 0) of
  Right a -> return a
  Left str -> putStrLn str >> fail ""

typeLit :: Literal -> TC Type
typeLit (LInt _) = pure $ Con "Int" []
typeLit (LDbl _) = pure $ Con "Double" []
typeLit (LStr _) = pure $ Con "String" []
typeLit (LJavaScript _) = freshVar

unbound :: Name -> TC Type
unbound name = do
  env <- ask
  case M.lookup name $ decVars $ tcDecs env of
    Just dec -> typeExpr dec
    Nothing -> case M.lookup name $ decRegistries $ tcDecs env of
      Just dec -> typeExpr (Case dec)
      Nothing -> typeError $ "Not in scope: " ++ name

refine :: Type -> TC Type
refine ty = with (\pp -> "refine: " ++ pp ty) $ do
  tc <- get
  let go e = e >>= \case
        TyVar i -> case IM.lookup i $ tcVars tc of
          Just t -> go t
          Nothing -> Var $ TyVar i
        VarName name -> Var $ VarName name
  return $ go ty

bindTypes :: [(Name, Type)] -> TC a -> TC a
bindTypes m = local (\tc -> tc
  { varTypeBinding = M.fromList m `M.union` varTypeBinding tc
  , tcStack = [v ++ " <- " ++ show t | (v, t) <- m] ++ tcStack tc })

withVar :: (Type -> TC a) -> TC a
withVar k = do
  i <- fresh
  let name = "$" ++ show i
  let t = Var $ TyVar i
  bindTypes [(name, t)] (k t)

typeExpr :: Expr Name -> TC Type
typeExpr expr = with (\pp -> "infer: " ++ pp (fmap VarName expr)) $ (>>=refine) $ case expr of
  Var name -> do
    env <- ask
    case M.lookup name $ varTypeBinding env of
      Just t -> pure t
      Nothing -> unbound name
  Lit l -> typeLit l
  a :$ b -> withVar $ \tRes -> do
    tFun <- typeExpr a
    tArg <- typeExpr b
    unifyType tFun $ Con "->" [tArg, tRes]
    return tRes
  Lam v a -> withVar $ \tArg -> do
    tBody <- bindTypes [(v, tArg)] $ typeExpr a
    return $ Con "->" [tArg, tBody]
  e ::: sig -> do
    t <- typeExpr e
    let t' = fmap VarName sig
    unifyType t t'
    return t'
  Forall v e -> do
    qv <- freshVar
    bindTypes [(v, qv)] $ typeExpr e
  Case clauses -> withVar $ \tArg -> withVar $ \tRes -> do
    env <- ask
    forM_ clauses $ \(con, vs, c) -> case M.lookup con $ decConstructors $ tcDecs env of
      Just (qs, ts, t) | length vs <= length ts -> flip runContT return $ do
        qvars <- replicateM (length qs) (ContT withVar)
        let subst' = foldr (uncurry subst) `flip` zip (map VarName qs) qvars
        let ts' = map (subst' . fmap VarName) ts
        lift $ unifyType tArg $ partialCon (subst' $ fmap VarName t) (drop (length vs) ts')
        lift $ bindTypes (zip vs ts') $ do
          typeExpr c >>= unifyType tRes
      Just _ -> typeError "constructor arguments mismatch"
      Nothing -> typeError "constructor not found"
    return $ Con "->" [tArg, tRes]
  Con name xs -> do
    env <- ask
    case M.lookup name $ decConstructors $ tcDecs env of
      Nothing -> typeError $ "Missing con definition: " ++ name
      Just (qs, ts, t) -> flip runContT return $ do
        qvars <- replicateM (length qs) (ContT withVar)
        let subst' = foldr (uncurry subst) `flip` zip (map VarName qs) qvars
        let ts' = map subst' $ map (fmap VarName) ts
        lift $ bindTypes (zip qs qvars) $ do
          ys <- traverse typeExpr xs
          sequence_ $ zipWith unifyType ts' ys
        return $ partialCon (subst' $ fmap VarName t) $ drop (length xs) ts'
  Coerce _ -> freshVar

partialCon :: Foldable t => Expr a -> t (Expr a) -> Expr a
partialCon = foldr (\a r -> Con "->" [a, r])

freshVar :: TC Type
freshVar = do
  i <- fresh
  return $ Var $ TyVar i
