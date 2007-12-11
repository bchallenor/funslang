module Typing where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Bits
import Control.Monad
import Representation
import Pretty
--import Debug.Trace


data Typeability a
  = Typeable !a
  | Untypeable !String
  
  deriving (Show, Eq)

instance Monad Typeability where
  (Typeable x) >>= k = k x
  (Untypeable s) >>= _ = Untypeable s
  return = Typeable
  fail = Untypeable

type Env = Map.Map String Type


--
-- Parameters
--

-- The number of bits, less the sign bit, allowed for Funslang integers
fs_UNSIGNED_INT_BITS :: Int
fs_UNSIGNED_INT_BITS = 23


--
-- Type inference
-- 

-- Wrapper (empty gamma)
inferType :: Monad m => Expr -> m TypedExpr
inferType = inferType' Map.empty

-- Helper
inferType' :: Monad m => Env -> Expr -> m TypedExpr

inferType' _ (UnitExpr) = do
  return (UnitTypedExpr)

inferType' _ (IntExpr i) = do
  if shiftR (abs i) fs_UNSIGNED_INT_BITS == 0
    then return (IntTypedExpr i)
    else fail ("integer <" ++ show (abs i) ++ "> too large for " ++ show fs_UNSIGNED_INT_BITS ++ " bits")

inferType' _ (FloatExpr d) = do
  return (FloatTypedExpr d)

inferType' _ (BoolExpr b) = do
  return (BoolTypedExpr b)

inferType' gamma (VarExpr s) = do
  case Map.lookup s gamma of
    Just t -> return (VarTypedExpr t s)
    Nothing -> fail ("unknown variable <" ++ s ++ ">")

inferType' gamma (AppOpExpr op' es) = do
  es' <- mapM (inferType' gamma) es
  let ts = map typeOf es'
  case specialize op' ts of
    Just t -> return (AppOpTypedExpr t op' es')
    Nothing -> fail ("type error specializing operator in <" ++ prettyExpr (AppOpExpr op' es) ++ ">")

inferType' gamma (AppFnExpr f x) = do
  f' <- inferType' gamma f
  x' <- inferType' gamma x
  case typeOf f' of
    FunType ta tb -> do
      if ta == typeOf x'
        then return (AppFnTypedExpr tb f' x')
        else fail ("<" ++ prettyExpr x ++ "> has type " ++ prettyType (typeOf x') ++ " but function expects type " ++ prettyType ta)
    _ -> fail ("<" ++ prettyExpr f ++ "> is not of function type")

inferType' gamma (ArrayExpr es) = do
  es' <- mapM (inferType' gamma) es
  case List.nub (map typeOf es') of
    [t] -> return (ArrayTypedExpr (ArrayType t (toInteger (length es'))) es')
    _ -> fail ("elements in array <" ++ prettyExpr (ArrayExpr es) ++ "> do not all have same type")

inferType' gamma (TupleExpr es) = do
  es' <- mapM (inferType' gamma) es
  return (TupleTypedExpr (TupleType (map typeOf es')) es')

inferType' gamma (IfExpr ec et ef) = do
  ec' <- inferType' gamma ec
  case typeOf ec' of
    BoolType -> do
      et' <- inferType' gamma et
      let tt = typeOf et'
      ef' <- inferType' gamma ef
      let tf = typeOf ef'
      if tt == tf
        then return (IfTypedExpr tt ec' et' ef')
        else fail ("arms of <" ++ prettyExpr (IfExpr ec et ef) ++ "> must have same type")
    _ -> fail ("condition of <" ++ prettyExpr (IfExpr ec et ef) ++ "> must have " ++ prettyType BoolType ++ " type")

inferType' gamma (LetExpr p bound body) = do -- todo: remove the redundancy between let-exprs and lambdas...
  bound' <- inferType' gamma bound
  let tp = typeOf bound'
  case matchTypeWithPattern tp p of
    Just tbindings -> do
      let (vs, _) = unzip tbindings
      case Set.size (Set.fromList vs) == length vs of
        True -> do
          let gamma' = List.foldl' (\m (k,v) -> Map.insert k v m) gamma tbindings
          body' <- inferType' gamma' body
          return (LetTypedExpr (typeOf body') p bound' body')
        False -> fail ("names not unique in pattern <" ++ prettyPatt p ++ ">")
    Nothing -> fail ("pattern <" ++ prettyPatt p ++ "> does not match bound expression <" ++ prettyExpr bound ++ "> with type <" ++ prettyType tp ++ ">")

inferType' gamma (LambdaExpr p tp body) = do
  case matchTypeWithPattern tp p of
    Just tbindings -> do
      let (vs, _) = unzip tbindings
      case Set.size (Set.fromList vs) == length vs of
        True -> do
          let gamma' = List.foldl' (\m (k,v) -> Map.insert k v m) gamma tbindings
          body' <- inferType' gamma' body
          return (LambdaTypedExpr (FunType tp (typeOf body')) p tp body')
        False -> fail ("names not unique in pattern <" ++ prettyPatt p ++ ">")
    Nothing -> fail ("pattern <" ++ prettyPatt p ++ "> does not match type <" ++ prettyType tp ++ ">")


--
-- Pattern matching functions
--

-- Generic error
patternMatchError :: [Char]
patternMatchError = "pattern match failure"


-- Finds the variable type bindings resulting from matching pattern to type
matchTypeWithPattern :: Monad m => Type -> Patt -> m [(String, Type)]

matchTypeWithPattern _ (WildPatt) = return []

matchTypeWithPattern (UnitType) (UnitPatt) = return []
matchTypeWithPattern _ (UnitPatt) = fail patternMatchError

matchTypeWithPattern t (VarPatt s) = return [(s,t)]

matchTypeWithPattern (ArrayType t i) (ArrayPatt ps) = do
  if i == toInteger (length ps)
    then do
      bs <- mapM (matchTypeWithPattern t) ps
      return (concat bs)
    else fail patternMatchError
matchTypeWithPattern _ (ArrayPatt ps) = fail patternMatchError

matchTypeWithPattern (TupleType ts) (TuplePatt ps) = do
  if length ts == length ps
    then do
      bs <- zipWithM matchTypeWithPattern ts ps
      return (concat bs)
    else fail patternMatchError
matchTypeWithPattern _ (TuplePatt ps) = fail patternMatchError


--
-- Type classification
--

isArithmeticScalarType :: Type -> Bool
isArithmeticScalarType t =
  case t of
    IntType -> True
    FloatType -> True
    _ -> False

isArithmeticVectorType :: Type -> Bool
isArithmeticVectorType t =
  case t of
    ArrayType IntType _ -> True
    ArrayType FloatType _ -> True
    _ -> False

isArithmeticMatrixType :: Type -> Bool
isArithmeticMatrixType t =
  case t of
    ArrayType (ArrayType IntType _) _ -> True
    ArrayType (ArrayType FloatType _) _ -> True
    _ -> False

isArithmeticType :: Type -> Bool
isArithmeticType t = isArithmeticScalarType t || isArithmeticVectorType t || isArithmeticMatrixType t

baseTypeOfArithmeticType :: Type -> Maybe Type
baseTypeOfArithmeticType t =
  case t of
    IntType -> Just IntType
    ArrayType IntType _ -> Just IntType
    ArrayType (ArrayType IntType _) _ -> Just IntType
    FloatType -> Just FloatType
    ArrayType FloatType _ -> Just FloatType
    ArrayType (ArrayType FloatType _) _ -> Just FloatType
    _ -> Nothing

isEqualityType :: Type -> Bool
isEqualityType t =
  case t of
    FunType _ _ -> False
    _ -> True


---
--- Operator specialization
---

-- Specialize the given operator with the given types to find the return type
specialize :: Op -> [Type] -> Maybe Type

-- i/f -> i/f; i/f n -> i/f n; i/f m n -> i/f m n
specialize (Op1Prefix' Op1Neg) ts = let [a] = ts in
  if isArithmeticType a then Just a else Nothing

-- bool -> bool
specialize (Op1Prefix' Op1Not) ts = let [a] = ts in
  if a == BoolType then Just a else Nothing

-- i/f m n -> i/f n m
specialize (Op1Postfix' Op1Transpose) ts = let [a] = ts in
  case a of
    ArrayType (ArrayType b m) n -> Just (ArrayType (ArrayType b n) m)
    _ -> Nothing

-- a n -> Int -> a
specialize (Op2Infix' Op2Subscript) ts = let [a,b] = ts in
  case a of
    ArrayType c n -> case b of
      IntType -> Just c
      _ -> Nothing
    _ -> Nothing

-- a n -> Int m -> a m
specialize (Op2Infix' Op2Swizzle) ts = let [a,b] = ts in
  case a of
    ArrayType c n -> case b of
      ArrayType IntType m -> Just (ArrayType c m)
      _ -> Nothing
    _ -> Nothing

-- a n -> a m -> a m+n
specialize (Op2Infix' Op2Append) ts = let [a,b] = ts in
  case a of
    ArrayType c n -> case b of
      ArrayType c' m -> if c == c' then Just (ArrayType c (m+n)) else Nothing
      _ -> Nothing
    _ -> Nothing

-- i/f -> i/f -> i/f; i/f n -> i/f n -> i/f n; i/f m n -> i/f m n -> i/f m n
specialize (Op2Infix' Op2Mul) ts = let [a,b] = ts in
  if a == b && isArithmeticType a && isArithmeticType b then Just a else Nothing
specialize (Op2Infix' Op2Div) ts = specialize (Op2Infix' Op2Mul) ts
specialize (Op2Infix' Op2Add) ts = specialize (Op2Infix' Op2Mul) ts
specialize (Op2Infix' Op2Sub) ts = specialize (Op2Infix' Op2Mul) ts

-- i/f q p -> i/f r q -> i/f r p; i/f q -> i/f r q -> i/f r; i/f q p -> i/f q -> i/f p
specialize (Op2Infix' Op2LinearMul) ts = let [a,b] = ts in
  if isArithmeticType a && isArithmeticType b
    then case a of
      ArrayType (ArrayType t q) p -> case b of
        ArrayType (ArrayType t' r) q' -> if q == q' && t == t'
          then Just (ArrayType (ArrayType t r) p)
          else Nothing
        ArrayType t' q' -> if q == q' && t == t'
          then Just (ArrayType t' p)
          else Nothing
        _ -> Nothing
      ArrayType t q -> case b of
        ArrayType (ArrayType t' r) q' -> if q == q' && t == t'
          then Just (ArrayType t r)
          else Nothing
        _ -> Nothing
      _ -> Nothing
    else Nothing

-- i/f n -> i/f -> i/f n; i/f m n -> i/f -> i/f m n
specialize (Op2Infix' Op2ScaleMul) ts = let [a,b] = ts in
  if isArithmeticVectorType a || isArithmeticMatrixType a
    then case baseTypeOfArithmeticType a of
      Just a' -> if a' == b
        then Just a
        else Nothing
      Nothing -> Nothing
    else Nothing
specialize (Op2Infix' Op2ScaleDiv) ts = specialize (Op2Infix' Op2ScaleMul) ts

-- i/f -> i/f -> bool
specialize (Op2Infix' Op2LessThan) ts = let [a,b] = ts in
  if a == b && isArithmeticScalarType a then Just BoolType else Nothing
specialize (Op2Infix' Op2LessThanEqual) ts = specialize (Op2Infix' Op2LessThan) ts
specialize (Op2Infix' Op2GreaterThan) ts = specialize (Op2Infix' Op2LessThan) ts
specialize (Op2Infix' Op2GreaterThanEqual) ts = specialize (Op2Infix' Op2LessThan) ts

-- Eq a => a -> a -> bool
specialize (Op2Infix' Op2Equal) ts = let [a,b] = ts in
  if a == b && isEqualityType a then Just BoolType else Nothing
specialize (Op2Infix' Op2NotEqual) ts = specialize (Op2Infix' Op2Equal) ts

-- bool -> bool -> bool
specialize (Op2Infix' Op2And) ts = let [a,b] = ts in
  if a == b && a == BoolType then Just BoolType else Nothing
specialize (Op2Infix' Op2Or) ts = specialize (Op2Infix' Op2And) ts

