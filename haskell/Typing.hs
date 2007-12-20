-- Particularly useful in writing the inference system was "Algorithm W Step by Step"
-- by Martin Grabmuller <http://uebb.cs.tu-berlin.de/~magr/pub/AlgorithmW.pdf>.
-- The principal type function, however, is based on that in the Types course
-- by Prof. Andrew M. Pitts.

module Typing(inferExprType, Scheme(..), SchemeEnv, fvType) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import Control.Monad.State
import Control.Monad.Error
import Pretty
import Representation


-- The type inference function. This is the main function for this module.
inferExprType :: SchemeEnv -> Expr -> ([TypeVarRef], [DimVarRef]) -> Either String (Type, ([TypeVarRef], [DimVarRef]))
inferExprType gamma e vrefs = do
  let (a,vrefs') = runState (runErrorT $ principalType gamma e) vrefs
  (s,t) <- a
  return (t, vrefs')


-- The type inference monad holds the fresh var ref state (use put/get),
-- and can return errors (use throwError).
type TI a = ErrorT String (State ([TypeVarRef], [DimVarRef])) a

--runTI :: TI a -> ([TypeVarRef], [DimVarRef]) -> (Either String a, ([TypeVarRef], [DimVarRef]))
--runTI ti state = runState (runErrorT ti) state


-- Take a fresh type variable.
freshTypeVar :: TI Type
freshTypeVar = do
  (fresh_tvref:fresh_tvrefs, fresh_dvrefs) <- get
  put (fresh_tvrefs, fresh_dvrefs)
  return $ TypeVar fresh_tvref

-- Take a fresh dim variable.
freshDimVar :: TI Dim
freshDimVar = do
  (fresh_tvrefs, fresh_dvref:fresh_dvrefs) <- get
  put (fresh_tvrefs, fresh_dvrefs)
  return $ DimVar fresh_dvref


-- A substitution binds type vars to types and dim vars to dims.
type TypeVarSubst = Map.Map TypeVarRef Type
type DimVarSubst = Map.Map DimVarRef Dim
type Subst = (TypeVarSubst, DimVarSubst)

-- VarRefs will represent sets of free or bound type and dim vars.
type VarRefs = (Set.Set TypeVarRef, Set.Set DimVarRef)

emptyVarRefs :: VarRefs
emptyVarRefs = (Set.empty, Set.empty)

unionVarRefs :: VarRefs -> VarRefs -> VarRefs
unionVarRefs (l1, r1) (l2, r2) = (Set.union l1 l2, Set.union r1 r2)

differenceVarRefs :: VarRefs -> VarRefs -> VarRefs
differenceVarRefs (l1, r1) (l2, r2) = (Set.difference l1 l2, Set.difference r1 r2)

-- Null substitutions.
nullTypeVarSubst :: TypeVarSubst
nullTypeVarSubst = Map.empty
nullDimVarSubst :: DimVarSubst
nullDimVarSubst = Map.empty
nullSubst :: Subst
nullSubst = (nullTypeVarSubst, nullDimVarSubst)

-- Dim variable substitution.
applyDimVarSubst :: DimVarSubst -> Dim -> Dim
applyDimVarSubst dsub d@(DimVar dvref) =
  case Map.lookup dvref dsub of
    Just d' -> d'
    Nothing -> d
applyDimVarSubst _ d = d -- all other dims are atoms, and map to themselves

fvType :: Type -> (Set.Set TypeVarRef, Set.Set DimVarRef)
fvType (TypeArray t (DimVar dvref)) = fvType t `unionVarRefs` (Set.empty, Set.singleton dvref)
fvType (TypeArray t (DimFix _)) = fvType t
fvType (TypeTuple ts) = List.foldl1' unionVarRefs (map fvType ts)
fvType (TypeFun t1 t2) = fvType t1 `unionVarRefs` fvType t2
fvType (TypeVar tvref) = (Set.singleton tvref, Set.empty)
fvType _ = (Set.empty, Set.empty) -- nothing else can contain a var

applySubstType :: Subst -> Type -> Type
applySubstType (tsub, dsub) (TypeArray t d) = TypeArray (applySubstType (tsub, dsub) t) (applyDimVarSubst dsub d)
applySubstType (tsub, dsub) (TypeTuple ts) = TypeTuple (map (applySubstType (tsub, dsub)) ts)
applySubstType (tsub, dsub) (TypeFun t1 t2) = TypeFun (applySubstType (tsub, dsub) t1) (applySubstType (tsub, dsub) t2)
applySubstType (tsub, _) t@(TypeVar tvref) =
  case Map.lookup tvref tsub of
    Just t' -> t'
    Nothing -> t
applySubstType (_, _) t = t -- all other types are atoms, and map to themselves

-- Substitution composition, finding s3 such that s3 t = s2(s1(t))
-- s3 contains:
-- a) all the bindings in s1 with s2 applied to their right hand sides
-- b) all the bindings in s2, except if they clash with a) in which case a) takes precendence
-- Note that Map.union prefers its first argument when duplicate keys are encountered.
composeSubst :: Subst -> Subst -> Subst
(tsub2, dsub2) `composeSubst` (tsub1, dsub1) =
  ((Map.map (applySubstType (tsub2, dsub2)) tsub1) `Map.union` tsub2, (Map.map (applyDimVarSubst dsub2) dsub1) `Map.union` dsub2)

-- Returns the substitution that binds a type variable to a type.
bindTypeVarRef :: TypeVarRef -> Type -> TI TypeVarSubst
bindTypeVarRef tvref t
  | TypeVar tvref == t = return nullTypeVarSubst -- identity substitutions should not fail occurs check
  | let (ftv, _) = fvType t in tvref `Set.member` ftv =
    let [tv, pt] = prettyTypes [TypeVar tvref, t] in
      throwError $ "occurs check: " ++ tv ++ " = " ++ pt -- oops
  | otherwise = return (Map.singleton tvref t) -- finally, we can do the bind

-- Returns the substitution that binds a dim variable to a dim.
bindDimVarRef :: DimVarRef -> Dim -> TI DimVarSubst
bindDimVarRef dvref d
  | DimVar dvref == d = return nullDimVarSubst
  | otherwise = return (Map.singleton dvref d)

-- Returns the most general unifier of two types.
mgu :: Type -> Type -> TI Subst
mgu (TypeUnit) (TypeUnit) = return nullSubst
mgu (TypeReal) (TypeReal) = return nullSubst
mgu (TypeBool) (TypeBool) = return nullSubst
mgu (TypeTexture1D) (TypeTexture1D) = return nullSubst
mgu (TypeTexture2D) (TypeTexture2D) = return nullSubst
mgu (TypeTexture3D) (TypeTexture3D) = return nullSubst
mgu (TypeTextureCube) (TypeTextureCube) = return nullSubst
mgu (TypeArray t (DimVar dvref)) (TypeArray t' d') = do
  dsub1 <- bindDimVarRef dvref d'
  let sub1 = (nullTypeVarSubst, dsub1)
  sub2 <- mgu (applySubstType sub1 t) (applySubstType sub1 t')
  return $ sub2 `composeSubst` sub1
mgu (TypeArray t d) (TypeArray t' (DimVar dvref')) =
  mgu (TypeArray t' (DimVar dvref')) (TypeArray t d) -- swap
mgu (TypeArray t (DimFix i)) (TypeArray t' (DimFix i')) =
  if i == i'
    then mgu t t'
    else throwError "array dimensions do not match"
mgu (TypeTuple ts) (TypeTuple ts') =
  if length ts == length ts'
    then Foldable.foldlM (
      \ sub1 (t, t') -> do
        sub2 <- mgu (applySubstType sub1 t) (applySubstType sub1 t')
        return $ sub2 `composeSubst` sub1
      ) nullSubst (zip ts ts')
    else throwError "tuple lengths do not match"
mgu (TypeFun t1 t2) (TypeFun t1' t2') = do
  sub1 <- mgu t1 t1'
  sub2 <- mgu (applySubstType sub1 t2) (applySubstType sub1 t2')
  return $ sub2 `composeSubst` sub1
mgu (TypeVar tvref) t' = do
  tsub <- bindTypeVarRef tvref t'
  return (tsub, nullDimVarSubst)
mgu t (TypeVar tvref') = do
  tsub <- bindTypeVarRef tvref' t
  return (tsub, nullDimVarSubst)
mgu t1 t2 =
  let [pt1, pt2] = prettyTypes [t1, t2] in
    throwError $ "could not unify <" ++ pt1 ++ "> with <" ++ pt2 ++ ">"


-- A type scheme generalizes a type over the bound type vars and dim vars.
data Scheme = Scheme !VarRefs !Type deriving (Show, Eq)

fvScheme :: Scheme -> (Set.Set TypeVarRef, Set.Set DimVarRef)
fvScheme (Scheme vrefs t) = fvType t `differenceVarRefs` vrefs

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme (tsub, dsub) (Scheme (tvrefs, dvrefs) t) =
  if (Set.null $ Map.keysSet tsub `Set.intersection` tvrefs) && (Set.null $ Map.keysSet dsub `Set.intersection` dvrefs)
    then Scheme (tvrefs, dvrefs) (applySubstType (tsub, dsub) t)
    else error "captured variable applying substitution to type scheme! var refs should be unique!"

-- Instantiate a type scheme to give a type.
instantiate :: Scheme -> TI Type
instantiate (Scheme (tvrefs, dvrefs) t) = do
  tsub <- Foldable.foldlM (
    \tsub tvref -> do
      tv <- freshTypeVar
      return $ Map.insert tvref tv tsub
    ) nullTypeVarSubst tvrefs
  dsub <- Foldable.foldlM (
    \dsub dvref -> do
      dv <- freshDimVar
      return $ Map.insert dvref dv dsub
    ) nullDimVarSubst dvrefs
  return $ applySubstType (tsub, dsub) t


-- A type environment maps identifiers to type schemes.
type SchemeEnv = Map.Map String Scheme

fvSchemeEnv :: SchemeEnv -> (Set.Set TypeVarRef, Set.Set DimVarRef)
fvSchemeEnv gamma = Map.fold (\sigma vrefs -> vrefs `unionVarRefs` fvScheme sigma) (Set.empty, Set.empty) gamma

applySubstSchemeEnv :: Subst -> SchemeEnv -> SchemeEnv
applySubstSchemeEnv sub gamma = Map.map (applySubstScheme sub) gamma


-- Type inference! The cool stuff.
-- Returns the principal type of the expression, and the substitution that must
-- be applied to gamma to achieve this.
principalType :: SchemeEnv -> Expr -> TI (Subst, Type)

principalType _ (ExprUnitLiteral) = return (nullSubst, TypeUnit)

principalType _ (ExprRealLiteral _) = return (nullSubst, TypeReal)

principalType _ (ExprBoolLiteral _) = return (nullSubst, TypeBool)

principalType gamma (ExprVar ident) =
  case Map.lookup ident gamma of
    Just sigma -> do
      t <- instantiate sigma
      return (nullSubst, t)
    Nothing -> throwError $ "unbound variable: " ++ ident

principalType gamma a@(ExprApp e1 e2) = do
  (s1, t1) <- principalType gamma e1
  let s1gamma = applySubstSchemeEnv s1 gamma
  (s2, t2) <- principalType s1gamma e2
  alpha <- freshTypeVar
  s3 <- mgu (applySubstType s2 t1) (TypeFun t2 alpha)
  return ((s3 `composeSubst` (s2 `composeSubst` s1)), applySubstType s3 alpha)
  `catchError` (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a)

principalType gamma a@(ExprArray es) = do
  alpha <- freshTypeVar -- the type of elements of the array
  (s1, s1gamma) <- Foldable.foldrM (
    \ e (s1, s1gamma) -> do
      (s2, t2) <- principalType s1gamma e
      s3 <- mgu t2 (applySubstType s1 alpha)
      return (s3 `composeSubst` (s2 `composeSubst` s1), applySubstSchemeEnv s3 (applySubstSchemeEnv s2 s1gamma))
    ) (nullSubst, gamma) es
  return (s1, TypeArray (applySubstType s1 alpha) (DimFix $ toInteger $ length es))
  `catchError` (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a)

principalType gamma a@(ExprTuple es) = do
  (s1, s1gamma, ts1) <- Foldable.foldrM (
    \ e (s1, s1gamma, ts1) -> do
      (s2, t2) <- principalType s1gamma e
      return (s2 `composeSubst` s1, applySubstSchemeEnv s2 s1gamma, t2:ts1)
    ) (nullSubst, gamma, []) es -- the empty tuple is invalid, but es has valid length
  return (s1, TypeTuple ts1)
  `catchError` (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a)

principalType gamma a@(ExprIf e1 e2 e3) = do
  (s1, t1) <- principalType gamma e1
  s2 <- mgu t1 TypeBool
  let s2s1gamma = applySubstSchemeEnv s2 (applySubstSchemeEnv s1 gamma)
  (s3, t3) <- principalType s2s1gamma e2
  let s3s2s1gamma = applySubstSchemeEnv s3 s2s1gamma
  (s4, t4) <- principalType s3s2s1gamma e3
  s5 <- mgu (applySubstType s4 t3) t4
  return (s5 `composeSubst` (s4 `composeSubst` (s3 `composeSubst` (s2 `composeSubst` s1))), applySubstType s5 t4)
  `catchError` (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a)

principalType gamma a@(ExprLet p e1 e2) = do
  (s1, t1) <- principalType gamma e1
  (t1', bindings) <- inferPattType p
  -- we should remove any mapping for the identifiers in p, because we're going to shadow them,
  -- and we don't want existing defs to stop us from generalizing their free variables
  let gamma_shadowed = Map.differenceWithKey (\ident _ _ -> Nothing) gamma bindings
  let s1gamma_shadowed = applySubstSchemeEnv s1 gamma_shadowed
  s2 <- mgu t1 t1'
  let s2s1gamma_shadowed = applySubstSchemeEnv s2 s1gamma_shadowed
  let generalize t = Scheme (fvType t `differenceVarRefs` fvSchemeEnv s2s1gamma_shadowed) t
  let s2bindingspoly = Map.map (generalize . applySubstType s2) bindings
  let gamma' = s2s1gamma_shadowed `Map.union` s2bindingspoly
  (s3, t3) <- principalType gamma' e2
  return (s3 `composeSubst` (s2 `composeSubst` s1), t3)
  `catchError` (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a)

principalType gamma a@(ExprLambda p e) = do
  (t1, m) <- inferPattType p
  -- generalize over nothing when converting to type schemes
  let params = Map.map (Scheme emptyVarRefs) m
  (s, t2) <- principalType (gamma `Map.union` params) e
  return (s, TypeFun (applySubstType s t1) t2)
  `catchError` (\s -> throwError $ s ++ "\nin expression: " ++ prettyExpr a)


-- Find a type for this pattern, and a mapping from identifiers to types.
inferPattType :: Patt -> TI (Type, Map.Map String Type)

inferPattType (PattWild (Just t)) =
  return (t, Map.empty)
inferPattType (PattWild Nothing) = do
  t <- freshTypeVar
  return (t, Map.empty)
  
inferPattType (PattUnit (Just t)) = do
  _ <- mgu t TypeUnit
  return (TypeUnit, Map.empty)
inferPattType (PattUnit Nothing) =
  return (TypeUnit, Map.empty)

inferPattType (PattVar ident (Just t)) =
  return (t, Map.singleton ident t)
inferPattType (PattVar ident Nothing) = do
  t <- freshTypeVar
  return (t, Map.singleton ident t)

inferPattType a@(PattArray ps (Just t)) = do
  telem <- freshTypeVar -- the element type
  s1 <- mgu t (TypeArray telem (DimFix $ toInteger $ length ps))
  let TypeArray telem' d' = applySubstType s1 t
  (acc_telem, acc_m) <- Foldable.foldrM (
    \ p (acc_telem, acc_m) -> do
      (this_telem, this_m) <- inferPattType p
      s2 <- mgu acc_telem this_telem
      if Map.null $ acc_m `Map.intersection` this_m
        then do
          let next_acc_m = acc_m `Map.union` this_m
          return (applySubstType s2 acc_telem, Map.map (applySubstType s2) next_acc_m)
        else throwError $ "names not unique in pattern: " ++ prettyPatt a
    ) (telem', Map.empty) ps
  return (TypeArray acc_telem d', acc_m)
inferPattType (PattArray ps Nothing) = do
  t <- freshTypeVar
  inferPattType $ PattArray ps (Just t)

inferPattType a@(PattTuple ps (Just t)) = do
  (acc_ts, acc_m) <- Foldable.foldrM (
    \ p (acc_ts, acc_m) -> do
      (this_t, this_m) <- inferPattType p
      if Map.null $ acc_m `Map.intersection` this_m
        then do
          let next_acc_m = acc_m `Map.union` this_m
          return (this_t:acc_ts, next_acc_m)
        else throwError $ "names not unique in pattern: " ++ prettyPatt a
    ) ([], Map.empty) ps
  s <- mgu t (TypeTuple acc_ts)
  return (applySubstType s t, Map.map (applySubstType s) acc_m)
inferPattType p@(PattTuple ps Nothing) = do
  t <- freshTypeVar
  inferPattType $ PattTuple ps (Just t)
