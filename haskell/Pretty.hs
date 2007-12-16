module Pretty(prettyType, prettyExpr, prettyPatt) where

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

prettyType :: Type -> String
prettyType = prettyExType . exTypeFromType

prettyExType :: ExType -> String
prettyExType (ExTypeUnit) = "()"
prettyExType (ExTypeReal) = "Real"
prettyExType (ExTypeBool) = "Bool"
prettyExType (ExTypeTexture1D) = "Texture1D"
prettyExType (ExTypeTexture2D) = "Texture2D"
prettyExType (ExTypeTexture3D) = "Texture3D"
prettyExType (ExTypeTextureCube) = "TextureCube"
prettyExType (ExTypeTuple dts) = tuple $ map prettyExType dts
prettyExType (ExTypeArray dt (ExDimFix i)) = paren (isExTypeFun dt) (prettyExType dt) ++ " " ++ show i
prettyExType (ExTypeFun dt1 dt2) = paren (isExTypeFun dt1) (prettyExType dt1) ++ " -> " ++ (prettyExType dt2)
prettyExType (ExTypeVar tv) = tv
prettyExType (ExTypeArray dt (ExDimVar dv)) = paren (isExTypeFun dt) (prettyExType dt) ++ " " ++ dv

-- We only need parens if the left subtype of a type is a function type.
isExTypeFun :: ExType -> Bool
isExTypeFun (ExTypeFun _ _) = True
isExTypeFun _ = False

prettyPatt :: Patt -> String
prettyPatt (WildPatt) = "_"
prettyPatt (UnitPatt) = "()"
prettyPatt (VarPatt s) = s
prettyPatt (ArrayPatt ps) = array (map prettyPatt ps)
prettyPatt (TuplePatt ps) = tuple (map prettyPatt ps)

prettyExpr :: Expr -> String
prettyExpr (UnitConstExpr) = "()"
prettyExpr (RealConstExpr d) = show d
prettyExpr (BoolConstExpr b) = show b
prettyExpr (VarExpr s) = s
prettyExpr (AppExpr e1 e2) = paren True $ paren True (prettyExpr e1) ++ " " ++ prettyExpr e2
prettyExpr (ArrayExpr es) = array (map prettyExpr es)
prettyExpr (TupleExpr es) = tuple (map prettyExpr es)
prettyExpr (IfExpr ec et ef) = "if " ++ prettyExpr ec ++ " then " ++ prettyExpr et ++ " else " ++ prettyExpr ef
prettyExpr (LetExpr p ea eb) = "let " ++ prettyPatt p ++ " = " ++ prettyExpr ea ++ " in " ++ prettyExpr eb
prettyExpr (LambdaExpr p e) = "\\ " ++ prettyPatt p ++ " . " ++ prettyExpr e
