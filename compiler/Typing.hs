-- Particularly useful in writing the inference system was "Algorithm W Step by Step"
-- by Martin Grabmuller <http://uebb.cs.tu-berlin.de/~magr/pub/AlgorithmW.pdf>.
-- The principal type function, however, is based on that in the Types course
-- by Prof. Andrew M. Pitts.

module Typing(inferExprType, inferNewEnv, fvType, TI, runTI, mgu, nullSubst, Subst, applySubstType, freshTypeVar, freshDimVar) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import Control.Monad.State
import Control.Monad.Error

import Representation
import CompileError


-- The type inference function. This is the main function for this module.
inferExprType :: SchemeEnv -> Expr -> ([TypeVarRef], [DimVarRef]) -> Either CompileError (Type, ([TypeVarRef], [DimVarRef]))
inferExprType gamma e vrefs = do
  ((_,t), vrefs') <- runTI (principalType gamma e) vrefs
  return (t, vrefs')

-- Debugging function which performs a binding, returning its type and the new environment.
inferNewEnv :: SchemeEnv -> Patt -> Expr -> ([TypeVarRef], [DimVarRef]) -> Either CompileError (Type, SchemeEnv, ([TypeVarRef], [DimVarRef]))
inferNewEnv gamma p e vrefs = do
  ((_,t,gamma'), vrefs') <- runTI (principalTypeBinding gamma p e) vrefs
  return (t, gamma', vrefs')


-- The type inference monad holds the fresh var ref state (use put/get),
-- and can return errors (use throwError).
type TI a = ErrorT CompileError (State ([TypeVarRef], [DimVarRef])) a

runTI :: TI a -> ([TypeVarRef], [DimVarRef]) -> Either CompileError (a, ([TypeVarRef], [DimVarRef]))
runTI ti vrefs = do
  let (a,vrefs') = runState (runErrorT ti) vrefs
  r <- a
  return (r, vrefs')


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
bindTypeVarRef tvref t =
  if TypeVar tvref == t
    then return nullTypeVarSubst -- identity substitutions should not fail occurs check
    else let (ftv, _) = fvType t in
      if tvref `Set.member` ftv
        then throwError $ TypeError [] $ TypeErrorOccursCheck (TypeVar tvref) t
        else return (Map.singleton tvref t) -- finally, we can do the bind

-- Returns the substitution that binds a dim variable to a dim.
bindDimVarRef :: DimVarRef -> Dim -> TI DimVarSubst
bindDimVarRef dvref d =
  if DimVar dvref == d
    then return nullDimVarSubst -- identity substitution
    else return (Map.singleton dvref d) -- no occurs check for dim vars


-- Returns the most general unifier of two types.
mgu :: Type -> Type -> TI Subst
mgu (TypeUnit) (TypeUnit) = return nullSubst
mgu (TypeReal) (TypeReal) = return nullSubst
mgu (TypeBool) (TypeBool) = return nullSubst
mgu a@(TypeTex tk) a'@(TypeTex tk') = if tk == tk' then return nullSubst else mguError a a'
mgu (TypeArray t (DimVar dvref)) (TypeArray t' d') = do
  dsub1 <- bindDimVarRef dvref d'
  let sub1 = (nullTypeVarSubst, dsub1)
  sub2 <- mgu (applySubstType sub1 t) (applySubstType sub1 t')
  return $ sub2 `composeSubst` sub1
mgu (TypeArray t d) (TypeArray t' (DimVar dvref')) =
  mgu (TypeArray t' (DimVar dvref')) (TypeArray t d) -- swap
mgu a@(TypeArray t (DimFix i)) a'@(TypeArray t' (DimFix i')) =
  if i == i'
    then mgu t t'
    else mguError a a'
mgu a@(TypeTuple ts) a'@(TypeTuple ts') =
  if length ts == length ts'
    then Foldable.foldlM (
      \ sub1 (t, t') -> do
        sub2 <- mgu (applySubstType sub1 t) (applySubstType sub1 t')
        return $ sub2 `composeSubst` sub1
      ) nullSubst (zip ts ts')
    else mguError a a'
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
mgu a a' = mguError a a'

mguError :: Type -> Type -> TI Subst
mguError a a' = throwError $ TypeError [] $ TypeErrorCouldNotUnify a a'


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

principalType gamma a@(ExprVar ident) = registerStackPoint a $
  case Map.lookup ident gamma of
    Just sigma -> do
      t <- instantiate sigma
      return (nullSubst, t)
    Nothing -> throwError $ TypeError [] $ TypeErrorUnboundVariable $ ident

principalType gamma a@(ExprApp e1 e2) = registerStackPoint a $ do
  (s1, t1) <- principalType gamma e1
  let s1gamma = applySubstSchemeEnv s1 gamma
  (s2, t2) <- principalType s1gamma e2
  alpha <- freshTypeVar
  s3 <- mgu (applySubstType s2 t1) (TypeFun t2 alpha)
  return ((s3 `composeSubst` (s2 `composeSubst` s1)), applySubstType s3 alpha)

principalType gamma a@(ExprArray es) = registerStackPoint a $ do
  alpha <- freshTypeVar -- the type of elements of the array
  (s1, _) <- Foldable.foldrM (
    \ e (s1, s1gamma) -> do
      (s2, t2) <- principalType s1gamma e
      s3 <- mgu t2 (applySubstType s1 alpha)
      return (s3 `composeSubst` (s2 `composeSubst` s1), applySubstSchemeEnv s3 (applySubstSchemeEnv s2 s1gamma))
    ) (nullSubst, gamma) es
  return (s1, TypeArray (applySubstType s1 alpha) (DimFix $ toInteger $ length es))

principalType gamma a@(ExprTuple es) = registerStackPoint a $ do
  (s1, _, ts1) <- Foldable.foldrM (
    \ e (s1, s1gamma, ts1) -> do
      (s2, t2) <- principalType s1gamma e
      return (s2 `composeSubst` s1, applySubstSchemeEnv s2 s1gamma, t2:ts1)
    ) (nullSubst, gamma, []) es -- the empty tuple is invalid, but es has valid length
  return (s1, TypeTuple ts1)

principalType gamma a@(ExprIf e1 e2 e3) = registerStackPoint a $ do
  (s1, t1) <- principalType gamma e1
  s2 <- mgu t1 TypeBool
  let s2s1gamma = applySubstSchemeEnv s2 (applySubstSchemeEnv s1 gamma)
  (s3, t3) <- principalType s2s1gamma e2
  let s3s2s1gamma = applySubstSchemeEnv s3 s2s1gamma
  (s4, t4) <- principalType s3s2s1gamma e3
  s5 <- mgu (applySubstType s4 t3) t4
  return (s5 `composeSubst` (s4 `composeSubst` (s3 `composeSubst` (s2 `composeSubst` s1))), applySubstType s5 t4)

principalType gamma a@(ExprLet p e1 e2) = registerStackPoint a $ do
  (s2s1, _, gamma') <- principalTypeBinding gamma p e1
  (s3, t3) <- principalType gamma' e2
  return (s3 `composeSubst` s2s1, t3)

principalType gamma a@(ExprLambda p e) = registerStackPoint a $ do
  (t1, m) <- inferPattType p
  -- generalize over nothing when converting to type schemes
  let params = Map.map (Scheme emptyVarRefs) m
  -- it is critical that Map.union prefers its first argument
  (s, t2) <- principalType (params `Map.union` gamma) e
  return (s, TypeFun (applySubstType s t1) t2)


-- Binds p to e, returning the substitution required and the new environment produced.
-- Helper function used both by normal let expressions and by debug let commands.
principalTypeBinding :: SchemeEnv -> Patt -> Expr -> TI (Subst, Type, SchemeEnv)
principalTypeBinding gamma p e = do
  (s1, t1) <- principalType gamma e
  (t1', bindings) <- inferPattType p
  -- we should remove any mapping for the identifiers in p, because we're going to shadow them,
  -- and we don't want existing defs to stop us from generalizing their free variables
  let gamma_shadowed = Map.differenceWithKey (\_ _ _ -> Nothing) gamma bindings
  let s1gamma_shadowed = applySubstSchemeEnv s1 gamma_shadowed
  s2 <- mgu t1 t1'
  let s2s1gamma_shadowed = applySubstSchemeEnv s2 s1gamma_shadowed
  let generalize t = Scheme (fvType t `differenceVarRefs` fvSchemeEnv s2s1gamma_shadowed) t
  let s2bindingspoly = Map.map (generalize . applySubstType s2) bindings
  let gamma' = s2s1gamma_shadowed `Map.union` s2bindingspoly
  return (s2 `composeSubst` s1, applySubstType s2 t1, gamma')


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
        else throwError $ TypeError [] $ TypeErrorDuplicateIdentsInPattern a
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
        else throwError $ TypeError [] $ TypeErrorDuplicateIdentsInPattern a
    ) ([], Map.empty) ps
  s <- mgu t (TypeTuple acc_ts)
  return (applySubstType s t, Map.map (applySubstType s) acc_m)
inferPattType (PattTuple ps Nothing) = do
  t <- freshTypeVar
  inferPattType $ PattTuple ps (Just t)
