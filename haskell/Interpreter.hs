module Interpreter where

import qualified Data.Map as Map
import Control.Monad.Error

import Representation


-- The environment of values as being interpreted.
type ValueEnv = Map.Map String Value


-- The inner interpreter function.
-- Returns the value, or on error, a message explaining the error and the expression that triggered it.
interpret' :: ValueEnv -> Expr -> Either String Value

interpret' env (ExprUnitLiteral) = return ValueUnit

interpret' env (ExprRealLiteral b) = return $ ValueDF $ DFRealLiteral b

interpret' env (ExprBoolLiteral b) = return $ ValueDF $ DFBoolLiteral b

interpret' env (ExprVar ident) =
  case Map.lookup ident env of
    Just v -> return v
    Nothing -> throwError $ "variable <" ++ ident ++ "> undefined"

interpret' env (ExprApp e1 e2) = do
  ValueFun f <- interpret' env e1
  v <- interpret' env e2
  f v

interpret' env (ExprArray es) = do
  vs <- mapM (interpret' env) es
  return $ ValueArray vs

interpret' env (ExprTuple es) = do
  vs <- mapM (interpret' env) es
  return $ ValueTuple vs

-- interpret' env (ExprIf eb e1 e2) = do
--   ValueBool dfb <- interpret' env e1
--   v1 <- interpret' env e1
--   v2 <- interpret' env e2
--   if v1 == v2
--     then return v1 -- optimize out if both branches are the same
--     else case dfb of
--       DFBoolLiteral b -> return $ if b then v1 else v2 -- optimize out if condition statically known
--       _ -> case v1 of -- so we have a runtime condition and runtime values
--         
--   
--   case dfb of
--     DFBoolLiteral b -> return $ if b then v1 else v2
--     _ -> case v1 of

--       (ValueReal dfr1) -> let ValueReal dfr2 = v2 in return $ DFRealCond dfr1 dfr2
--       (ValueBool dfb1) -> let ValueBool dfb2 = v2 in return $ DFBoolCond dfb1 dfb2
--       (ValueArray vs1) -> let ValueArray vs2 = v2 in return $ ValueArray $ map 
--       (ValueTuple vs) ->

--   
--   
--   case v1 of



-- interpret' env (ExprLet p e1 e2)
-- interpret' env (ExprLambda p e)


-- a = ValueFun (\(ValueReal dfr1) -> ValueFun (\(ValueReal dfr2) -> ValueReal (DFRealAdd dfr1 dfr2)))
-- app (ValueFun f) v = f v

-- b = app (app a (ValueReal (DFRealLiteral 1))) (ValueReal (DFRealLiteral 2))