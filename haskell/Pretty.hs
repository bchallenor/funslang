module Pretty(prettyOp, prettyType, prettyExpr, prettyPatt) where

import Data.List as List
import Representation

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

prettyOp :: Op -> String
prettyOp (Op1Prefix' Op1Neg)                  = "-"
prettyOp (Op1Prefix' Op1Not)                  = "~"
prettyOp (Op1Postfix' Op1Transpose)           = "'"
prettyOp (Op2Infix' Op2Subscript)             = "!"
prettyOp (Op2Infix' Op2Swizzle)               = "!!"
prettyOp (Op2Infix' Op2Append)                = "@"
prettyOp (Op2Infix' Op2Mul)                   = "*"
prettyOp (Op2Infix' Op2Div)                   = "/"
prettyOp (Op2Infix' Op2LinearMul)             = "**"
prettyOp (Op2Infix' Op2ScaleMul)              = "*."
prettyOp (Op2Infix' Op2ScaleDiv)              = "/."
prettyOp (Op2Infix' Op2Add)                   = "+"
prettyOp (Op2Infix' Op2Sub)                   = "-"
prettyOp (Op2Infix' Op2LessThan)              = "<"
prettyOp (Op2Infix' Op2LessThanEqual)         = "< ="
prettyOp (Op2Infix' Op2GreaterThan)           = ">"
prettyOp (Op2Infix' Op2GreaterThanEqual)      = "> ="
prettyOp (Op2Infix' Op2Equal)                 = " = ="
prettyOp (Op2Infix' Op2NotEqual)              = "/ ="
prettyOp (Op2Infix' Op2And)                   = "&&"
prettyOp (Op2Infix' Op2Or)                    = "||"

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
prettyExpr (AppOpExpr op' es) =
  let prefix = space $ map (paren True) (prettyOp op' : map prettyExpr es) in
  case op' of
    Op1Prefix' op -> prefix
    Op1Postfix' op -> let [e] = es in paren True (prettyExpr e) ++ prettyOp op'
    Op2Infix' op -> let [e1,e2] = es in paren True $ paren True (prettyExpr e1) ++ prettyOp op' ++ paren True (prettyExpr e2)
prettyExpr (AppFnExpr e1 e2) = paren True (prettyExpr e1) ++ prettyExpr e2
prettyExpr (ArrayExpr es) = array (map prettyExpr es)
prettyExpr (TupleExpr es) = tuple (map prettyExpr es)
prettyExpr (IfExpr ec et ef) = "if " ++ prettyExpr ec ++ " then " ++ prettyExpr et ++ " else " ++ prettyExpr ef
prettyExpr (LetExpr p ea eb) = "let " ++ prettyPatt p ++ " = " ++ prettyExpr ea ++ " in " ++ prettyExpr eb
prettyExpr (LambdaExpr p t e) = "\\ " ++ prettyPatt p ++ " :: " ++ prettyType t ++ " . " ++ prettyExpr e
