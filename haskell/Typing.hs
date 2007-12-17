-- Particularly useful in writing the inference system was "Algorithm W Step by Step"
-- by Martin Grabmuller <http://uebb.cs.tu-berlin.de/~magr/pub/AlgorithmW.pdf>

module Typing where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import Control.Monad.State
import Control.Monad.Error
import Pretty
import Representation


-- The type inference monad holds the fresh var ref state (use put/get),
-- and can return errors (use throwError).
type TI a = ErrorT String (State ([TypeVarRef], [DimVarRef])) a

runTI :: TI a -> ([TypeVarRef], [DimVarRef]) -> Either String a
runTI ti state = evalState (runErrorT ti) state


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

unionVarRefs :: VarRefs -> VarRefs -> VarRefs
unionVarRefs (l1, r1) (l2, r2) = (Set.union l1 l2, Set.union r1 r2)

differenceVarRefs :: VarRefs -> VarRefs -> VarRefs
differenceVarRefs (l1, r1) (l2, r2) = (Set.difference l1 l2, Set.difference r1 r2)

-- We'll want to overload these functions for Type, Scheme, and Env.
class ContainsTypeDimVars a where
  fv :: a -> VarRefs
  applySubst :: Subst -> a -> a

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
applyDimVarSubst dsub d = d -- all other dims are atoms, and map to themselves

-- Type variable substitution.
applyTypeVarSubst :: TypeVarSubst -> Type -> Type
applyTypeVarSubst tsub = applySubst (tsub, nullDimVarSubst)

-- We can now provide the ContainsTypeDimVars functions for Type.
instance ContainsTypeDimVars Type where

  fv (TypeArray t (DimVar dvref)) = fv t `unionVarRefs` (Set.empty, Set.singleton dvref)
  fv (TypeArray t (DimFix i)) = fv t
  fv (TypeTuple ts) = List.foldl1' unionVarRefs (map fv ts)
  fv (TypeFun t1 t2) = fv t1 `unionVarRefs` fv t2
  fv (TypeVar tvref) = (Set.singleton tvref, Set.empty)
  fv _ = (Set.empty, Set.empty) -- nothing else can contain a var
  
  applySubst (tsub, dsub) (TypeArray t d) = TypeArray (applySubst (tsub, dsub) t) (applyDimVarSubst dsub d)
  applySubst (tsub, dsub) (TypeTuple ts) = TypeTuple (map (applySubst (tsub, dsub)) ts)
  applySubst (tsub, dsub) (TypeFun t1 t2) = TypeFun (applySubst (tsub, dsub) t1) (applySubst (tsub, dsub) t2)
  applySubst (tsub, dsub) t@(TypeVar tvref) =
    case Map.lookup tvref tsub of
      Just t' -> t'
      Nothing -> t
  applySubst (tsub, dsub) t = t -- all other types are atoms, and map to themselves

-- Substitution composition, finding s3 such that s3 t = s2(s1(t))
-- s3 contains:
-- a) all the bindings in s1 with s2 applied to their right hand sides
-- b) all the bindings in s2, except if they clash with a) in which case a) takes precendence
-- Note that Map.union prefers its first argument when duplicate keys are encountered.
composeSubst :: Subst -> Subst -> Subst
(tsub2, dsub2) `composeSubst` (tsub1, dsub1) =
  ((Map.map (applyTypeVarSubst tsub2) tsub1) `Map.union` tsub2, (Map.map (applyDimVarSubst dsub2) dsub1) `Map.union` dsub2)

-- Returns the substitution that binds a type variable to a type.
bindTypeVarRef :: TypeVarRef -> Type -> TI TypeVarSubst
bindTypeVarRef tvref t
  | TypeVar tvref == t = return nullTypeVarSubst -- identity substitutions should not fail occurs check
  | let (ftv, fdv) = fv t in tvref `Set.member` ftv =
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
  sub2 <- mgu (applySubst sub1 t) (applySubst sub1 t')
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
        sub2 <- mgu (applySubst sub1 t) (applySubst sub1 t')
        return $ sub2 `composeSubst` sub1
      ) nullSubst (zip ts ts')
    else throwError "tuple lengths do not match"
mgu (TypeFun t1 t2) (TypeFun t1' t2') = do
  sub1 <- mgu t1 t1'
  sub2 <- mgu (applySubst sub1 t2) (applySubst sub1 t2')
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
data Scheme = Scheme !VarRefs !Type

instance ContainsTypeDimVars Scheme where

  fv (Scheme vrefs t) = fv t `differenceVarRefs` vrefs

  applySubst (tsub, dsub) (Scheme (tvrefs, dvrefs) t) =
    let tsub' = Set.fold Map.delete tsub tvrefs in
    let dsub' = Set.fold Map.delete dsub dvrefs in
      Scheme (tvrefs, dvrefs) (applySubst (tsub', dsub') t)


-- A type environment maps identifiers to type schemes.
data Env = Gamma !(Map.Map String Scheme)

instance ContainsTypeDimVars Env where
  
  fv (Gamma env) = List.foldl' unionVarRefs (Set.empty, Set.empty) (map fv (Map.elems env))

  applySubst sub (Gamma env) = Gamma (Map.map (applySubst sub) env)

remove :: Env -> String -> Env
remove (Gamma env) ident = Gamma (Map.delete ident env)


-- Generalize a type to a type scheme (given an environment, allowing us to avoid capture).
generalize :: Env -> Type -> Scheme
generalize gamma t = Scheme (fv t `differenceVarRefs` fv gamma) t

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
  return $ applySubst (tsub, dsub) t


-- Type inference! The cool stuff.
-- Returns the principal type of the expression, and the substitution that must
-- be applied to gamma to achieve this.
-- principalType :: Env -> Expr -> TI (Subst, Type)
-- principalType _ (UnitConstExpr) = (nullSubst, TypeUnit)
-- principalType _ (RealConstExpr _)
-- principalType _ (BoolConstExpr _)
-- principalType (VarExpr ident)
-- principalType (AppExpr e1 e2)
-- principalType (ArrayExpr es)
-- principalType (TupleExpr es)
-- principalType (IfExpr eb e1 e2)
-- principalType (LetExpr (VarPatt ident) ebound ebody)
-- principalType (LambdaExpr (VarPatt ident) ebody)



