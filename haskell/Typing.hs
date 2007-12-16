-- Particularly useful in writing the inference system was "Algorithm W Step by Step"
-- by Martin Grabmuller <http://uebb.cs.tu-berlin.de/~magr/pub/AlgorithmW.pdf>

module Typing where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import Pretty
import Representation


-- todo: come up with a better way of doing this
data Typeability a
  = Typeable !a
  | Untypeable !String
  
  deriving (Show, Eq)

instance Monad Typeability where
  (Typeable x) >>= k = k x
  (Untypeable s) >>= _ = Untypeable s
  return = Typeable
  fail = Untypeable



-- A type scheme generalizes a type over the type vars and dim vars.
data Scheme = Scheme ![TypeVarRef] ![DimVarRef] !Type

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
applyDimVarSubst dsub d = d -- all other dims are atoms, and map to themselves

-- Type variable substitution.
applyTypeVarSubst :: TypeVarSubst -> Type -> Type
applyTypeVarSubst tsub = applySubst (tsub, nullDimVarSubst)

-- Type and dim variable substitution.
applySubst :: Subst -> Type -> Type
applySubst (tsub, dsub) (TypeArray t d) = TypeArray (applySubst (tsub, dsub) t) (applyDimVarSubst dsub d)
applySubst (tsub, dsub) (TypeTuple ts) = TypeTuple (map (applySubst (tsub, dsub)) ts)
applySubst (tsub, dsub) (TypeFun t1 t2) = TypeFun (applySubst (tsub, dsub) t1) (applySubst (tsub, dsub) t2)
applySubst (tsub, dsub) t@(TypeVar tvref) =
  case Map.lookup tvref tsub of
    Just t' -> t'
    Nothing -> t
applySubst (tsub, dsub) t = t -- all other types are atoms, and map to themselves

-- Substitution composition, finding s3 such that s3 t = s1(s2(t))
-- s3 contains:
-- a) all the bindings in s2 with s1 applied to their right hand sides
-- b) all the bindings in s1, except if they clash with a) in which case a) takes precendence
-- Note that Map.union prefers its first argument when duplicate keys are encountered.
composeSubst :: Subst -> Subst -> Subst
composeSubst (tsub1, dsub1) (tsub2, dsub2) =
  ((Map.map (applyTypeVarSubst tsub1) tsub2) `Map.union` tsub1, (Map.map (applyDimVarSubst dsub1) dsub2) `Map.union` dsub1)

-- Finds the free type variables in a type.
ftv :: Type -> Set.Set TypeVarRef
ftv (TypeArray t d) = ftv t
ftv (TypeTuple ts) = List.foldl1' Set.union (map ftv ts)
ftv (TypeFun t1 t2) = ftv t1 `Set.union` ftv t2
ftv (TypeVar tvref) = Set.singleton tvref
ftv _ = Set.empty -- nothing else can contain a var

-- Returns the substitution that binds a type variable to a type.
bindTypeVarRef :: Monad m => TypeVarRef -> Type -> m TypeVarSubst
bindTypeVarRef tvref t
  | TypeVar tvref == t = return nullTypeVarSubst -- identity substitutions should not fail occurs check
  | tvref `Set.member` ftv t =
    let [tv, pt] = prettyTypes [TypeVar tvref, t] in
    fail $ "occurs check: " ++ tv ++ " = " ++ pt -- oops
  | otherwise = return (Map.singleton tvref t) -- finally, we can do the bind

-- Finds the free dim variables in a dim.
fdv :: Dim -> Set.Set DimVarRef
fdv (DimVar dvref) = Set.singleton dvref
fdv _ = Set.empty -- nothing else can contain a var

-- Returns the substitution that binds a dim variable to a dim.
bindDimVarRef :: Monad m => DimVarRef -> Dim -> m DimVarSubst
bindDimVarRef dvref d
  | DimVar dvref == d = return nullDimVarSubst
  | otherwise = return (Map.singleton dvref d)

-- Returns the most general unifier of two types.
mgu :: Type -> Type -> Typeability Subst
mgu (TypeUnit) (TypeUnit) = return nullSubst
mgu (TypeReal) (TypeReal) = return nullSubst
mgu (TypeBool) (TypeBool) = return nullSubst
mgu (TypeTexture1D) (TypeTexture1D) = return nullSubst
mgu (TypeTexture2D) (TypeTexture2D) = return nullSubst
mgu (TypeTexture3D) (TypeTexture3D) = return nullSubst
mgu (TypeTextureCube) (TypeTextureCube) = return nullSubst
mgu (TypeArray t (DimVar dvref)) (TypeArray t' d') = do
  dsub1 <- bindDimVarRef dvref d'
  (tsub2, dsub2) <- mgu (applySubst (nullTypeVarSubst, dsub1) t) (applySubst (nullTypeVarSubst, dsub1) t')
  return $ (tsub2, dsub2) `composeSubst` (nullTypeVarSubst, dsub1)
mgu (TypeArray t d) (TypeArray t' (DimVar dvref')) =
  mgu (TypeArray t' (DimVar dvref')) (TypeArray t d) -- swap
mgu (TypeArray t (DimFix i)) (TypeArray t' (DimFix i')) =
  if i == i'
    then mgu t t'
    else fail "array dimensions do not match"
mgu (TypeTuple ts) (TypeTuple ts') =
  if length ts == length ts'
    then Foldable.foldlM (
      \sub1 (t, t') -> do
        sub2 <- mgu (applySubst sub1 t) (applySubst sub1 t')
        return $ sub2 `composeSubst` sub1
      ) nullSubst (zip ts ts')
    else fail "tuple lengths do not match"
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
  fail $ "could not unify <" ++ pt1 ++ "> with <" ++ pt2 ++ ">"







{- import qualified Data.Map as Map
 - import qualified Data.List as List
 - import qualified Data.Set as Set
 - import Data.Bits
 - import Control.Monad
 - import Representation
 - import Pretty
 - --import Debug.Trace
 - 
 - 
 - data Typeability a
 -   = Typeable !a
 -   | Untypeable !String
 -   
 -   deriving (Show, Eq)
 - 
 - instance Monad Typeability where
 -   (Typeable x) >>= k = k x
 -   (Untypeable s) >>= _ = Untypeable s
 -   return = Typeable
 -   fail = Untypeable
 - 
 - type Env = Map.Map String Type
 - 
 - 
 - --
 - -- Parameters
 - --
 - 
 - -- The number of bits, less the sign bit, allowed for Funslang integers
 - fs_UNSIGNED_INT_BITS :: Int
 - fs_UNSIGNED_INT_BITS = 23
 - 
 - 
 - --
 - -- Type inference
 - -- 
 - 
 - -- Wrapper (empty gamma)
 - inferType :: Monad m => Expr -> m TypedExpr
 - inferType = inferType' Map.empty
 - 
 - -- Helper
 - inferType' :: Monad m => Env -> Expr -> m TypedExpr
 - 
 - inferType' _ (UnitExpr) = do
 -   return (UnitTypedExpr)
 - 
 - inferType' _ (IntExpr i) = do
 -   if shiftR (abs i) fs_UNSIGNED_INT_BITS == 0
 -     then return (IntTypedExpr i)
 -     else fail ("integer <" ++ show (abs i) ++ "> too large for " ++ show fs_UNSIGNED_INT_BITS ++ " bits")
 - 
 - inferType' _ (FloatExpr d) = do
 -   return (FloatTypedExpr d)
 - 
 - inferType' _ (BoolExpr b) = do
 -   return (BoolTypedExpr b)
 - 
 - inferType' gamma (VarExpr s) = do
 -   case Map.lookup s gamma of
 -     Just t -> return (VarTypedExpr t s)
 -     Nothing -> fail ("unknown variable <" ++ s ++ ">")
 - 
 - inferType' gamma (AppOpExpr op' es) = do
 -   es' <- mapM (inferType' gamma) es
 -   let ts = map typeOf es'
 -   case specialize op' ts of
 -     Just t -> return (AppOpTypedExpr t op' es')
 -     Nothing -> fail ("type error specializing operator in <" ++ prettyExpr (AppOpExpr op' es) ++ ">")
 - 
 - inferType' gamma (AppFnExpr f x) = do
 -   f' <- inferType' gamma f
 -   x' <- inferType' gamma x
 -   case typeOf f' of
 -     FunType ta tb -> do
 -       if ta == typeOf x'
 -         then return (AppFnTypedExpr tb f' x')
 -         else fail ("<" ++ prettyExpr x ++ "> has type " ++ prettyType (typeOf x') ++ " but function expects type " ++ prettyType ta)
 -     _ -> fail ("<" ++ prettyExpr f ++ "> is not of function type")
 - 
 - inferType' gamma (ArrayExpr es) = do
 -   es' <- mapM (inferType' gamma) es
 -   case List.nub (map typeOf es') of
 -     [t] -> return (ArrayTypedExpr (ArrayType t (toInteger (length es'))) es')
 -     _ -> fail ("elements in array <" ++ prettyExpr (ArrayExpr es) ++ "> do not all have same type")
 - 
 - inferType' gamma (TupleExpr es) = do
 -   es' <- mapM (inferType' gamma) es
 -   return (TupleTypedExpr (TupleType (map typeOf es')) es')
 - 
 - inferType' gamma (IfExpr ec et ef) = do
 -   ec' <- inferType' gamma ec
 -   case typeOf ec' of
 -     BoolType -> do
 -       et' <- inferType' gamma et
 -       let tt = typeOf et'
 -       ef' <- inferType' gamma ef
 -       let tf = typeOf ef'
 -       if tt == tf
 -         then return (IfTypedExpr tt ec' et' ef')
 -         else fail ("arms of <" ++ prettyExpr (IfExpr ec et ef) ++ "> must have same type")
 -     _ -> fail ("condition of <" ++ prettyExpr (IfExpr ec et ef) ++ "> must have " ++ prettyType BoolType ++ " type")
 - 
 - inferType' gamma (LetExpr p bound body) = do -- todo: remove the redundancy between let-exprs and lambdas...
 -   bound' <- inferType' gamma bound
 -   let tp = typeOf bound'
 -   case matchTypeWithPattern tp p of
 -     Just tbindings -> do
 -       let (vs, _) = unzip tbindings
 -       case Set.size (Set.fromList vs) == length vs of
 -         True -> do
 -           let gamma' = List.foldl' (\m (k,v) -> Map.insert k v m) gamma tbindings
 -           body' <- inferType' gamma' body
 -           return (LetTypedExpr (typeOf body') p bound' body')
 -         False -> fail ("names not unique in pattern <" ++ prettyPatt p ++ ">")
 -     Nothing -> fail ("pattern <" ++ prettyPatt p ++ "> does not match bound expression <" ++ prettyExpr bound ++ "> with type <" ++ prettyType tp ++ ">")
 - 
 - inferType' gamma (LambdaExpr p tp body) = do
 -   case matchTypeWithPattern tp p of
 -     Just tbindings -> do
 -       let (vs, _) = unzip tbindings
 -       case Set.size (Set.fromList vs) == length vs of
 -         True -> do
 -           let gamma' = List.foldl' (\m (k,v) -> Map.insert k v m) gamma tbindings
 -           body' <- inferType' gamma' body
 -           return (LambdaTypedExpr (FunType tp (typeOf body')) p tp body')
 -         False -> fail ("names not unique in pattern <" ++ prettyPatt p ++ ">")
 -     Nothing -> fail ("pattern <" ++ prettyPatt p ++ "> does not match type <" ++ prettyType tp ++ ">")
 - 
 - 
 - --
 - -- Pattern matching functions
 - --
 - 
 - -- Generic error
 - patternMatchError :: [Char]
 - patternMatchError = "pattern match failure"
 - 
 - 
 - -- Finds the variable type bindings resulting from matching pattern to type
 - matchTypeWithPattern :: Monad m => Type -> Patt -> m [(String, Type)]
 - 
 - matchTypeWithPattern _ (WildPatt) = return []
 - 
 - matchTypeWithPattern (UnitType) (UnitPatt) = return []
 - matchTypeWithPattern _ (UnitPatt) = fail patternMatchError
 - 
 - matchTypeWithPattern t (VarPatt s) = return [(s,t)]
 - 
 - matchTypeWithPattern (ArrayType t i) (ArrayPatt ps) = do
 -   if i == toInteger (length ps)
 -     then do
 -       bs <- mapM (matchTypeWithPattern t) ps
 -       return (concat bs)
 -     else fail patternMatchError
 - matchTypeWithPattern _ (ArrayPatt ps) = fail patternMatchError
 - 
 - matchTypeWithPattern (TupleType ts) (TuplePatt ps) = do
 -   if length ts == length ps
 -     then do
 -       bs <- zipWithM matchTypeWithPattern ts ps
 -       return (concat bs)
 -     else fail patternMatchError
 - matchTypeWithPattern _ (TuplePatt ps) = fail patternMatchError
 - 
 - 
 - --
 - -- Type classification
 - --
 - 
 - isArithmeticScalarType :: Type -> Bool
 - isArithmeticScalarType t =
 -   case t of
 -     IntType -> True
 -     FloatType -> True
 -     _ -> False
 - 
 - isArithmeticVectorType :: Type -> Bool
 - isArithmeticVectorType t =
 -   case t of
 -     ArrayType IntType _ -> True
 -     ArrayType FloatType _ -> True
 -     _ -> False
 - 
 - isArithmeticMatrixType :: Type -> Bool
 - isArithmeticMatrixType t =
 -   case t of
 -     ArrayType (ArrayType IntType _) _ -> True
 -     ArrayType (ArrayType FloatType _) _ -> True
 -     _ -> False
 - 
 - isArithmeticType :: Type -> Bool
 - isArithmeticType t = isArithmeticScalarType t || isArithmeticVectorType t || isArithmeticMatrixType t
 - 
 - baseTypeOfArithmeticType :: Type -> Maybe Type
 - baseTypeOfArithmeticType t =
 -   case t of
 -     IntType -> Just IntType
 -     ArrayType IntType _ -> Just IntType
 -     ArrayType (ArrayType IntType _) _ -> Just IntType
 -     FloatType -> Just FloatType
 -     ArrayType FloatType _ -> Just FloatType
 -     ArrayType (ArrayType FloatType _) _ -> Just FloatType
 -     _ -> Nothing
 - 
 - isEqualityType :: Type -> Bool
 - isEqualityType t =
 -   case t of
 -     FunType _ _ -> False
 -     _ -> True
 - 
 - 
 - ---
 - --- Operator specialization
 - ---
 - 
 - -- Specialize the given operator with the given types to find the return type
 - specialize :: Op -> [Type] -> Maybe Type
 - 
 - -- i/f -> i/f; i/f n -> i/f n; i/f m n -> i/f m n
 - specialize (Op1Prefix' Op1Neg) ts = let [a] = ts in
 -   if isArithmeticType a then Just a else Nothing
 - 
 - -- bool -> bool
 - specialize (Op1Prefix' Op1Not) ts = let [a] = ts in
 -   if a == BoolType then Just a else Nothing
 - 
 - -- i/f m n -> i/f n m
 - specialize (Op1Postfix' Op1Transpose) ts = let [a] = ts in
 -   case a of
 -     ArrayType (ArrayType b m) n -> Just (ArrayType (ArrayType b n) m)
 -     _ -> Nothing
 - 
 - -- a n -> Int -> a
 - specialize (Op2Infix' Op2Subscript) ts = let [a,b] = ts in
 -   case a of
 -     ArrayType c n -> case b of
 -       IntType -> Just c
 -       _ -> Nothing
 -     _ -> Nothing
 - 
 - -- a n -> Int m -> a m
 - specialize (Op2Infix' Op2Swizzle) ts = let [a,b] = ts in
 -   case a of
 -     ArrayType c n -> case b of
 -       ArrayType IntType m -> Just (ArrayType c m)
 -       _ -> Nothing
 -     _ -> Nothing
 - 
 - -- a n -> a m -> a m+n
 - specialize (Op2Infix' Op2Append) ts = let [a,b] = ts in
 -   case a of
 -     ArrayType c n -> case b of
 -       ArrayType c' m -> if c == c' then Just (ArrayType c (m+n)) else Nothing
 -       _ -> Nothing
 -     _ -> Nothing
 - 
 - -- i/f -> i/f -> i/f; i/f n -> i/f n -> i/f n; i/f m n -> i/f m n -> i/f m n
 - specialize (Op2Infix' Op2Mul) ts = let [a,b] = ts in
 -   if a == b && isArithmeticType a && isArithmeticType b then Just a else Nothing
 - specialize (Op2Infix' Op2Div) ts = specialize (Op2Infix' Op2Mul) ts
 - specialize (Op2Infix' Op2Add) ts = specialize (Op2Infix' Op2Mul) ts
 - specialize (Op2Infix' Op2Sub) ts = specialize (Op2Infix' Op2Mul) ts
 - 
 - -- i/f q p -> i/f r q -> i/f r p; i/f q -> i/f r q -> i/f r; i/f q p -> i/f q -> i/f p
 - specialize (Op2Infix' Op2LinearMul) ts = let [a,b] = ts in
 -   if isArithmeticType a && isArithmeticType b
 -     then case a of
 -       ArrayType (ArrayType t q) p -> case b of
 -         ArrayType (ArrayType t' r) q' -> if q == q' && t == t'
 -           then Just (ArrayType (ArrayType t r) p)
 -           else Nothing
 -         ArrayType t' q' -> if q == q' && t == t'
 -           then Just (ArrayType t' p)
 -           else Nothing
 -         _ -> Nothing
 -       ArrayType t q -> case b of
 -         ArrayType (ArrayType t' r) q' -> if q == q' && t == t'
 -           then Just (ArrayType t r)
 -           else Nothing
 -         _ -> Nothing
 -       _ -> Nothing
 -     else Nothing
 - 
 - -- i/f n -> i/f -> i/f n; i/f m n -> i/f -> i/f m n
 - specialize (Op2Infix' Op2ScaleMul) ts = let [a,b] = ts in
 -   if isArithmeticVectorType a || isArithmeticMatrixType a
 -     then case baseTypeOfArithmeticType a of
 -       Just a' -> if a' == b
 -         then Just a
 -         else Nothing
 -       Nothing -> Nothing
 -     else Nothing
 - specialize (Op2Infix' Op2ScaleDiv) ts = specialize (Op2Infix' Op2ScaleMul) ts
 - 
 - -- i/f -> i/f -> bool
 - specialize (Op2Infix' Op2LessThan) ts = let [a,b] = ts in
 -   if a == b && isArithmeticScalarType a then Just BoolType else Nothing
 - specialize (Op2Infix' Op2LessThanEqual) ts = specialize (Op2Infix' Op2LessThan) ts
 - specialize (Op2Infix' Op2GreaterThan) ts = specialize (Op2Infix' Op2LessThan) ts
 - specialize (Op2Infix' Op2GreaterThanEqual) ts = specialize (Op2Infix' Op2LessThan) ts
 - 
 - -- Eq a => a -> a -> bool
 - specialize (Op2Infix' Op2Equal) ts = let [a,b] = ts in
 -   if a == b && isEqualityType a then Just BoolType else Nothing
 - specialize (Op2Infix' Op2NotEqual) ts = specialize (Op2Infix' Op2Equal) ts
 - 
 - -- bool -> bool -> bool
 - specialize (Op2Infix' Op2And) ts = let [a,b] = ts in
 -   if a == b && a == BoolType then Just BoolType else Nothing
 - specialize (Op2Infix' Op2Or) ts = specialize (Op2Infix' Op2And) ts
 - 
 - -- (a -> b) -> a n -> b n
 - specialize (Op2Prefix' Op2_map) ts = let [p,q] = ts in
 -   case p of
 -     FunType a b -> case q of
 -       ArrayType a' n -> if a == a'
 -         then Just (ArrayType b n)
 -         else Nothing
 -       _ -> Nothing
 -     _ -> Nothing
 - 
 - -- (a -> b -> a) -> a -> b n -> a
 - specialize (Op3Prefix' Op3_foldl) ts = let [p,q,r] = ts in
 -   case p of
 -     FunType a (FunType b a') -> if q == a && q == a'
 -       then case r of
 -         ArrayType b' _ -> if b == b'
 -           then Just a
 -           else Nothing
 -         _ -> Nothing
 -       else Nothing
 -     _ -> Nothing
 - 
 - -- (a -> b -> b) -> b -> a n -> b
 - specialize (Op3Prefix' Op3_foldr) ts = let [p,q,r] = ts in
 -   case p of
 -     FunType a (FunType b b') -> if q == b && q == b'
 -       then case r of
 -         ArrayType a' _ -> if a == a'
 -           then Just b
 -           else Nothing
 -         _ -> Nothing
 -       else Nothing
 -     _ -> Nothing
 -}
