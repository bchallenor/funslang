module Pretty(prettyExpr, prettyType, prettyPatt) where

import Data.List as List
import Representation
import Library

paren :: Bool -> [Char] -> [Char]
paren True s = "(" ++ s ++ ")"
paren False s = s

brack :: Bool -> [Char] -> [Char]
brack True s = "[" ++ s ++ "]"
brack False s = s

comma :: [[Char]] -> [Char]
comma = concat . List.intersperse ", "

space :: [[Char]] -> [Char]
space = concat . List.intersperse " "

tuple :: [[Char]] -> [Char]
tuple = paren True . comma

array :: [[Char]] -> [Char]
array = brack True . comma

prettyError :: String
prettyError = "invalid data passed to Pretty module"

prettyOp1 :: Operator1 -> String
prettyOp1 Op1Transpose            = "'"
prettyOp1 Op1Neg                  = "-"
prettyOp1 Op1Not                  = "~"

prettyOp2 :: Operator2 -> String
prettyOp2 Op2Subscript            = "!"
prettyOp2 Op2Swizzle              = "!!"
prettyOp2 Op2Append               = "@"
prettyOp2 Op2Mul                  = "*"
prettyOp2 Op2Div                  = "/"
prettyOp2 Op2LinearMul            = "**"
prettyOp2 Op2ScaleMul             = "*."
prettyOp2 Op2ScaleDiv             = "/."
prettyOp2 Op2Add                  = "+"
prettyOp2 Op2Sub                  = "-"
prettyOp2 Op2LessThan             = "<"
prettyOp2 Op2LessThanEqual        = "<="
prettyOp2 Op2GreaterThan          = ">"
prettyOp2 Op2GreaterThanEqual     = ">="
prettyOp2 Op2Equal                = "=="
prettyOp2 Op2NotEqual             = "/="
prettyOp2 Op2Identical            = "==="
prettyOp2 Op2NotIdentical         = "/=="
prettyOp2 Op2And                  = "&&"
prettyOp2 Op2Or                   = "||"

prettyType :: Type -> String
prettyType (UnitType) = "()"
prettyType (IntType) = "Int"
prettyType (FloatType) = "Float"
prettyType (BoolType) = "Bool"
prettyType (Texture1DType) = "Texture1D"
prettyType (Texture2DType) = "Texture2D"
prettyType (Texture3DType) = "Texture3D"
prettyType (TextureCubeType) = "TextureCube"
prettyType (TupleType ts) = tuple (map prettyType ts)
prettyType (ArrayType t i) =
  let s b = paren b (prettyType t) ++ " " ++ show i in
  case t of
    ArrayType _ _ -> s False
    _ -> s $ not $ atomType t
prettyType (FunType t1 t2) =
  let s b = paren b (prettyType t1) ++ " -> " ++ prettyType t2 in
    s $ not $ atomType t1

atomType :: Type -> Bool
atomType (UnitType) = True
atomType (IntType) = True
atomType (FloatType) = True
atomType (BoolType) = True
atomType (Texture1DType) = True
atomType (Texture2DType) = True
atomType (Texture3DType) = True
atomType (TextureCubeType) = True
atomType (TupleType ts) = True
atomType (ArrayType t i) = False
atomType (FunType t1 t2) = False

prettyPatt :: Patt -> String
prettyPatt (WildPatt) = "_"
prettyPatt (UnitPatt) = "()"
prettyPatt (VarPatt s) = s
prettyPatt (ArrayPatt ps) = array (map prettyPatt ps)
prettyPatt (TuplePatt ps) = tuple (map prettyPatt ps)

prettyExpr :: Expr -> String
prettyExpr (UnitExpr) = "()"
prettyExpr (IntExpr i) = show i
prettyExpr (FloatExpr d) = show d
prettyExpr (BoolExpr b) = show b
prettyExpr (VarExpr s) = s
prettyExpr (AppOp1Expr op e1) = space (map (paren True) [prettyOp1 op, prettyExpr e1])
prettyExpr (AppOp2Expr op e1 e2) = space (map (paren True) [prettyOp2 op, prettyExpr e1, prettyExpr e2])
prettyExpr (AppExpr e1 e2) = paren True (prettyExpr e1) ++ prettyExpr e2
prettyExpr (ArrayExpr es) = array (map prettyExpr es)
prettyExpr (TupleExpr es) = tuple (map prettyExpr es)
prettyExpr (IfExpr ec et ef) = "if " ++ prettyExpr ec ++ " then " ++ prettyExpr et ++ " else " ++ prettyExpr ef
prettyExpr (LetExpr p ea eb) = "let " ++ prettyPatt p ++ " = " ++ prettyExpr ea ++ " in " ++ prettyExpr eb
prettyExpr (LambdaExpr p t e) = "\\ " ++ prettyPatt p ++ " :: " ++ prettyType t ++ " . " ++ prettyExpr e
