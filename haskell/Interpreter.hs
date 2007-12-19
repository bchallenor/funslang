module Interpreter where

import Representation

a = ValueFun (\(ValueReal dfr1) -> ValueFun (\(ValueReal dfr2) -> ValueReal (DFRealAdd dfr1 dfr2)))
app (ValueFun f) v = f v

b = app (app a (ValueReal (DFRealLiteral 1))) (ValueReal (DFRealLiteral 2))


-- interpret :: Expr -> Value
-- interpret (ExprUnitLiteral) = ValueUnit
-- interpret (ExprRealLiteral b) = 
-- interpret (ExprBoolLiteral b)
-- interpret (ExprVar ident)
-- interpret (ExprApp e1 e2)
-- interpret (ExprArray es)
-- interpret (ExprTuple es)
-- interpret (ExprIf e1 e2 e3)
-- interpret (ExprLet p e1 e2)
-- interpret (ExprLambda p e)