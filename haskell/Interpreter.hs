module Interpreter(interpretExpr, ValueEnv) where

import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.Error

import Representation


-- The environment of values as being interpreted.
type ValueEnv = Map.Map String Value


-- The inner interpreter function.
-- Returns the value, or on error, a message explaining the error and the expression that triggered it.
interpretExpr :: ValueEnv -> Expr -> Either String Value

interpretExpr _ (ExprUnitLiteral) = return ValueUnit

interpretExpr _ (ExprRealLiteral b) = return $ ValueDF $ DFRealLiteral b

interpretExpr _ (ExprBoolLiteral b) = return $ ValueDF $ DFBoolLiteral b

interpretExpr env (ExprVar ident) =
  case Map.lookup ident env of
    Just v -> return v
    Nothing -> throwError $ "variable <" ++ ident ++ "> undefined"

interpretExpr env (ExprApp e1 e2) = do
  ValueFun f <- interpretExpr env e1
  v <- interpretExpr env e2
  f v

interpretExpr env (ExprArray es) = do
  vs <- mapM (interpretExpr env) es
  return $ ValueArray vs

interpretExpr env (ExprTuple es) = do
  vs <- mapM (interpretExpr env) es
  return $ ValueTuple vs

interpretExpr env (ExprIf eb e1 e2) = do
  ValueDF dfb <- interpretExpr env eb
  v1 <- interpretExpr env e1
  v2 <- interpretExpr env e2
  if v1 == v2
    then return v1 -- optimize out if both branches are the same
    else case dfb of
      DFBoolLiteral b -> return $ if b then v1 else v2 -- optimize out if condition statically known
      _ -> conditionalize dfb v1 v2 -- runtime condition and runtime values

interpretExpr env (ExprLet p e1 e2) = do
  v1 <- interpretExpr env e1
  let env' = env `Map.union` matchPattern p v1
  v2 <- interpretExpr env' e2
  return v2

interpretExpr env (ExprLambda p e) =
  return $ ValueFun (\v -> let env' = env `Map.union` matchPattern p v in interpretExpr env' e)


-- Takes a boolean condition and zips up the two values with DFCond nodes,
-- so that the resulting value is either the first or second value according to the condition.
conditionalize :: DF -> Value -> Value -> Either String Value
conditionalize dfb (ValueDF df1) (ValueDF df2) = return $ ValueDF $ DFCond (dfType df1) dfb df1 df2
conditionalize dfb (ValueArray vs1) (ValueArray vs2) = do
  vs <- zipWithM (conditionalize dfb) vs1 vs2
  return $ ValueArray vs
conditionalize dfb (ValueTuple vs1) (ValueTuple vs2) = do
  vs <- zipWithM (conditionalize dfb) vs1 vs2
  return $ ValueTuple vs
conditionalize _ _ _ = throwError $ "values in if expression cannot be compared at run time"


-- Match the pattern against the value to give a value environment.
matchPattern :: Patt -> Value -> ValueEnv
matchPattern (PattWild _) _ = Map.empty
matchPattern (PattUnit _) _ = Map.empty
matchPattern (PattVar ident _) v = Map.singleton ident v
matchPattern (PattArray ps _) (ValueArray vs) = List.foldl1' Map.union (zipWith matchPattern ps vs)
matchPattern (PattArray _ _) _ = undefined
matchPattern (PattTuple ps _) (ValueTuple vs) = List.foldl1' Map.union (zipWith matchPattern ps vs)
matchPattern (PattTuple _ _) _ = undefined
