module Pretty where

import Data.List as List
import Representation

prettyOp :: Operator -> String
prettyOp OpSubscript            = "!"
prettyOp OpSwizzle              = "!!"
prettyOp OpAppend               = "@"
prettyOp OpTranspose            = "'"
prettyOp OpNeg                  = "-"
prettyOp OpNot                  = "~"
prettyOp OpMul                  = "*"
prettyOp OpDiv                  = "/"
prettyOp OpLinearMul            = "**"
prettyOp OpScaleMul             = "*."
prettyOp OpScaleDiv             = "/."
prettyOp OpAdd                  = "+"
prettyOp OpSub                  = "-"
prettyOp OpLessThan             = "<"
prettyOp OpLessThanEqual        = "<="
prettyOp OpGreaterThan          = ">"
prettyOp OpGreaterThanEqual     = ">="
prettyOp OpEqual                = "=="
prettyOp OpNotEqual             = "/="
prettyOp OpIdentical            = "==="
prettyOp OpNotIdentical         = "/=="
prettyOp OpAnd                  = "&&"
prettyOp OpOr                   = "||"

prettyPatt :: Patt -> String
prettyPatt (VarPatt s) = s
prettyPatt (ArrayPatt ps) = ('[':) $ (++"]") $ concat $ List.intersperse ", " $ map prettyPatt ps
prettyPatt (TuplePatt ps) = ('(':) $ (++")") $ concat $ List.intersperse ", " $ map prettyPatt ps

prettyType :: Type -> String
prettyType (UnitType) = "()"
prettyType (IntType) = "Int"
prettyType (FloatType) = "Float"
prettyType (BoolType) = "Bool"
prettyType (Texture1DType) = "Texture1D"
prettyType (Texture2DType) = "Texture2D"
prettyType (Texture3DType) = "Texture3D"
prettyType (TextureCubeType) = "TextureCube"
prettyType (TupleType ts) = ('[':) $ (++"]") $ concat $ List.intersperse ", " $ map prettyType ts
prettyType (ArrayType t i) = "(" ++ prettyType t ++ ")" ++ " " ++ show i
prettyType (FunType t1 t2) = "(" ++ prettyType t1 ++ ")" ++ " -> " ++ prettyType t2


prettyTypedIdent :: TypedIdent -> String
prettyTypedIdent (s, t) = s ++ " :: " ++ prettyType t

prettyExpr :: Expr -> String
prettyExpr (UnitExpr) = "()"
prettyExpr (IntExpr i) = show i
prettyExpr (FloatExpr d) = show d
prettyExpr (BoolExpr b) = show b
prettyExpr (VarExpr s) = s
prettyExpr (AppOpExpr op e) = "(" ++ prettyOp op ++ ") " ++ prettyExpr e
prettyExpr (AppExpr e1 e2) = "(" ++ prettyExpr e1 ++ ") " ++ prettyExpr e2
prettyExpr (ArrayExpr es) = ('[':) $ (++"]") $ concat $ List.intersperse ", " $ map prettyExpr es
prettyExpr (TupleExpr es) = ('(':) $ (++")") $ concat $ List.intersperse ", " $ map prettyExpr es
prettyExpr (IfExpr ec et ef) = "if " ++ prettyExpr ec ++ " then " ++ prettyExpr et ++ " else " ++ prettyExpr ef
prettyExpr (LetExpr p ea eb) = "let " ++ prettyPatt p ++ " = " ++ prettyExpr ea ++ " in " ++ prettyExpr eb
prettyExpr (LambdaExpr tids e) = "\\ (" ++ (concat $ List.intersperse ", " $ map prettyTypedIdent tids) ++ ") -> " ++ prettyExpr e