module Typing where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
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
-- Type inference
-- 

-- Wrapper (empty gamma)
inferType :: Monad m => Expr -> m TypedExpr
inferType = inferType' Map.empty

-- Helper
inferType' :: Monad m => Env -> Expr -> m TypedExpr

inferType' _ (UnitExpr) = return (UnitTypedExpr)

inferType' _ (IntExpr i) = return (IntTypedExpr i)

inferType' _ (FloatExpr d) = return (FloatTypedExpr d)

inferType' _ (BoolExpr b) = return (BoolTypedExpr b)

inferType' gamma (VarExpr s) = do
  case Map.lookup s gamma of
    Just t -> return (VarTypedExpr t s)
    Nothing -> fail ("unknown variable <" ++ s ++ ">")

-- inferType' _ (AppOp1Expr _ _) = do
--   fail "todo" -- todo

-- inferType' _ (AppOp2Expr _ _ _) = do
--   fail "todo" -- todo

inferType' gamma (AppExpr f x) = do
  f' <- inferType' gamma f
  x' <- inferType' gamma x
  case typeOf f' of
    FunType ta tb -> do
      if ta == typeOf x'
        then return (AppTypedExpr tb f' x')
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

inferType' gamma (LetExpr p bound body) = do
  bound' <- inferType' gamma bound
  case matchTypedExprWithPattern bound' p of
    Just ebindings -> do
      let tbindings = map (\(k,e) -> (k, typeOf e)) ebindings
      let (vs, _) = unzip tbindings
      case Set.size (Set.fromList vs) == length vs of
        True -> do
          let gamma' = List.foldl' (\m (k,v) -> Map.insert k v m) gamma tbindings
          body' <- inferType' gamma' body
          return (LetTypedExpr (typeOf body') ebindings body')
        False -> fail ("names not unique in pattern <" ++ prettyPatt p ++ ">")
    Nothing -> fail ("pattern <" ++ prettyPatt p ++ "> does not match bound expression <" ++ prettyExpr bound ++ ">")

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


-- Finds the variable bindings for a given pattern and typed expression
matchTypedExprWithPattern :: Monad m => TypedExpr -> Patt -> m [(String, TypedExpr)]

matchTypedExprWithPattern (UnitTypedExpr) (UnitPatt) = return []
matchTypedExprWithPattern _ (UnitPatt) = fail patternMatchError

matchTypedExprWithPattern e (VarPatt s) = return [(s,e)]

matchTypedExprWithPattern (ArrayTypedExpr t es) (ArrayPatt ps) = do
  if length es == length ps
    then do
      bs <- zipWithM matchTypedExprWithPattern es ps
      return (concat bs)
    else fail patternMatchError
matchTypedExprWithPattern _ (ArrayPatt ps) = fail patternMatchError

matchTypedExprWithPattern (TupleTypedExpr t es) (TuplePatt ps) = do
  if length es == length ps
    then do
      bs <- zipWithM matchTypedExprWithPattern es ps
      return (concat bs)
    else fail patternMatchError
matchTypedExprWithPattern _ (TuplePatt ps) = fail patternMatchError


-- Checks that a given pattern could have the given type
matchTypeWithPattern :: Monad m => Type -> Patt -> m [(String, Type)]

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
