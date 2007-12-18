-- Particularly useful in writing the inference system was "Algorithm W Step by Step"
-- by Martin Grabmuller <http://uebb.cs.tu-berlin.de/~magr/pub/AlgorithmW.pdf>.
-- The principal type function, however, is based on that in the Types course
-- by Prof. Andrew M. Pitts.

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

emptyVarRefs :: VarRefs
emptyVarRefs = (Set.empty, Set.empty)

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
applyDimVarSubst _ d = d -- all other dims are atoms, and map to themselves

-- We can now provide the ContainsTypeDimVars functions for Type.
instance ContainsTypeDimVars Type where

  fv (TypeArray t (DimVar dvref)) = fv t `unionVarRefs` (Set.empty, Set.singleton dvref)
  fv (TypeArray t (DimFix _)) = fv t
  fv (TypeTuple ts) = List.foldl1' unionVarRefs (map fv ts)
  fv (TypeFun t1 t2) = fv t1 `unionVarRefs` fv t2
  fv (TypeVar tvref) = (Set.singleton tvref, Set.empty)
  fv _ = (Set.empty, Set.empty) -- nothing else can contain a var
  
  applySubst (tsub, dsub) (TypeArray t d) = TypeArray (applySubst (tsub, dsub) t) (applyDimVarSubst dsub d)
  applySubst (tsub, dsub) (TypeTuple ts) = TypeTuple (map (applySubst (tsub, dsub)) ts)
  applySubst (tsub, dsub) (TypeFun t1 t2) = TypeFun (applySubst (tsub, dsub) t1) (applySubst (tsub, dsub) t2)
  applySubst (tsub, _) t@(TypeVar tvref) =
    case Map.lookup tvref tsub of
      Just t' -> t'
      Nothing -> t
  applySubst (_, _) t = t -- all other types are atoms, and map to themselves

-- Substitution composition, finding s3 such that s3 t = s2(s1(t))
-- s3 contains:
-- a) all the bindings in s1 with s2 applied to their right hand sides
-- b) all the bindings in s2, except if they clash with a) in which case a) takes precendence
-- Note that Map.union prefers its first argument when duplicate keys are encountered.
composeSubst :: Subst -> Subst -> Subst
(tsub2, dsub2) `composeSubst` (tsub1, dsub1) =
  ((Map.map (applySubst (tsub2, dsub2)) tsub1) `Map.union` tsub2, (Map.map (applyDimVarSubst dsub2) dsub1) `Map.union` dsub2)

-- Returns the substitution that binds a type variable to a type.
bindTypeVarRef :: TypeVarRef -> Type -> TI TypeVarSubst
bindTypeVarRef tvref t
  | TypeVar tvref == t = return nullTypeVarSubst -- identity substitutions should not fail occurs check
  | let (ftv, _) = fv t in tvref `Set.member` ftv =
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
data Scheme = Scheme !VarRefs !Type deriving (Show, Eq)

instance ContainsTypeDimVars Scheme where

  fv (Scheme vrefs t) = fv t `differenceVarRefs` vrefs

  applySubst (tsub, dsub) (Scheme (tvrefs, dvrefs) t) =
    let captureErrorMsg = "captured variable applying substitution to type scheme! var refs should be unique!" in
    if 0 /= (Set.size $ Map.keysSet tsub `Set.intersection` tvrefs)
      then error captureErrorMsg
      else if 0 /= (Set.size $ Map.keysSet dsub `Set.intersection` dvrefs)
        then error captureErrorMsg
        else Scheme (tvrefs, dvrefs) (applySubst (tsub, dsub) t)

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


-- A type environment maps identifiers to type schemes.
newtype Env = Env (Map.Map String Scheme) deriving (Show, Eq)

instance ContainsTypeDimVars Env where
  
  fv (Env m) = Map.fold (\t vrefs -> vrefs `unionVarRefs` fv t) (Set.empty, Set.empty) m

  applySubst sub (Env m) = Env (Map.map (applySubst sub) m)

envUnion :: Env -> Env -> Env
envUnion (Env m1) (Env m2) = Env (m1 `Map.union` m2)

removeIdent :: String -> Env -> Env
removeIdent ident (Env m) = Env (Map.delete ident m)

insertIdent :: String -> Scheme -> Env -> Env
insertIdent ident sigma (Env m) = Env (Map.insert ident sigma m)


-- Type inference! The cool stuff.
-- Returns the principal type of the expression, and the substitution that must
-- be applied to gamma to achieve this.
principalType :: Env -> Expr -> TI (Subst, Type)
principalType _ (ExprUnitLiteral) = return (nullSubst, TypeUnit)
principalType _ (ExprRealLiteral _) = return (nullSubst, TypeReal)
principalType _ (ExprBoolLiteral _) = return (nullSubst, TypeBool)
principalType (Env m) (ExprVar ident) =
  case Map.lookup ident m of
    Just sigma -> do
      t <- instantiate sigma
      return (nullSubst, t)
    Nothing -> throwError $ "unbound variable: " ++ ident
principalType gamma (ExprApp e1 e2) = do
  (s1, t1) <- principalType gamma e1
  let s1gamma = applySubst s1 gamma
  (s2, t2) <- principalType s1gamma e2
  alpha <- freshTypeVar
  s3 <- mgu (applySubst s2 t1) (TypeFun t2 alpha)
  return ((s3 `composeSubst` (s2 `composeSubst` s1)), applySubst s3 alpha)
principalType gamma (ExprArray es) = do
  alpha <- freshTypeVar -- the type of elements of the array
  (s1, s1gamma) <- Foldable.foldrM (
    \ e (s1, s1gamma) -> do
      (s2, t2) <- principalType s1gamma e
      s3 <- mgu t2 (applySubst s1 alpha)
      return (s3 `composeSubst` (s2 `composeSubst` s1), applySubst s3 (applySubst s2 s1gamma))
    ) (nullSubst, gamma) es
  return (s1, TypeArray (applySubst s1 alpha) (DimFix $ toInteger $ length es))
principalType gamma (ExprTuple es) = do
  (s1, s1gamma, ts1) <- Foldable.foldrM (
    \ e (s1, s1gamma, ts1) -> do
      (s2, t2) <- principalType s1gamma e
      return (s2 `composeSubst` s1, applySubst s2 s1gamma, t2:ts1)
    ) (nullSubst, gamma, []) es -- the empty tuple is invalid, but es has valid length
  return (s1, TypeTuple ts1)
principalType gamma (ExprIf e1 e2 e3) = do
  (s1, t1) <- principalType gamma e1
  s2 <- mgu t1 TypeBool
  let s2s1gamma = applySubst s2 (applySubst s1 gamma)
  (s3, t3) <- principalType s2s1gamma e2
  let s3s2s1gamma = applySubst s3 s2s1gamma
  (s4, t4) <- principalType s3s2s1gamma e3
  s5 <- mgu (applySubst s4 t3) t4
  return ((s5 `composeSubst` (s4 `composeSubst` (s3 `composeSubst` (s2 `composeSubst` s1)))), applySubst s5 t4)
principalType gamma (ExprLet (PattVar ident _) e1 e2) = do
  (s1, t1) <- principalType gamma e1
  -- we should remove any mapping for ident now, because we're going to shadow it,
  -- and we don't want it to stop us from generalizing variables that are free in it
  let gamma' = removeIdent ident gamma
  let s1gamma' = applySubst s1 gamma'
  let a = fv t1 `differenceVarRefs` fv s1gamma'
  (s2, t2) <- principalType (insertIdent ident (Scheme a t1) s1gamma') e2
  return (s2 `composeSubst` s1, t2)
principalType gamma (ExprLambda p e) = do
  (t1, m) <- inferPattType p
  -- generalize over nothing when converting to type schemes
  let params = Env $ Map.map (Scheme emptyVarRefs) m
  (s, t2) <- principalType (gamma `envUnion` params) e
  return (s, TypeFun (applySubst s t1) t2)


-- Find a type for this pattern, and a mapping from identifiers to types.

inferPattTypeErrorMsg :: Patt -> String
inferPattTypeErrorMsg p = "bad type annotation in pattern: " ++ prettyPatt p

inferPattType :: Patt -> TI (Type, Map.Map String Type)

inferPattType (PattWild (Just t)) =
  return (t, Map.empty)
inferPattType (PattWild Nothing) = do
  t <- freshTypeVar
  return (t, Map.empty)
  
inferPattType (PattUnit (Just TypeUnit)) =
  return (TypeUnit, Map.empty)
inferPattType (PattUnit Nothing) =
  return (TypeUnit, Map.empty)
inferPattType a@(PattUnit _) =
  throwError $ inferPattTypeErrorMsg a -- todo: change this

inferPattType (PattVar ident (Just t)) =
  return (t, Map.singleton ident t)
inferPattType (PattVar ident Nothing) = do
  t <- freshTypeVar
  return (t, Map.singleton ident t)

inferPattType a@(PattArray ps (Just t)) = do
  telem <- freshTypeVar -- the element type
  s1 <- mgu t (TypeArray telem (DimFix $ toInteger $ length ps))
  let t'@(TypeArray telem' _) = applySubst s1 t
  (acct, accm) <- Foldable.foldrM (
    \ p (acct, accm) -> do
      (next_telem, m) <- inferPattType p
      s2 <- mgu telem' next_telem
      if Map.null $ accm `Map.intersection` m
        then do
          let accm' = accm `Map.union` m
          return (applySubst s2 acct, Map.map (applySubst s2) accm')
        else throwError $ "names not unique in pattern: " ++ prettyPatt a
    ) (t', Map.empty) ps
  return (acct, accm)
inferPattType (PattArray ps Nothing) = do
  t <- freshTypeVar
  inferPattType $ PattArray ps (Just t)





-- inferPattType p@(PattTuple ps (Just t))
-- inferPattType p@(PattTuple ps Nothing)

