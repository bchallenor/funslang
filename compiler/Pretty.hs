module Pretty where

import qualified Data.List as List
import qualified Data.Set as Set
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

-- Prints a type in a fresh context.
prettyType :: Type -> String
prettyType = prettyExType . exTypeFromType

-- Prints a list of types in the same fresh context.
prettyTypes :: [Type] -> [String]
prettyTypes = map prettyExType . exTypesFromTypes

-- Optionally prints a type specifier.
prettyTypeSpecifier :: Maybe Type -> String
prettyTypeSpecifier (Just t) = " :: " ++ prettyType t
prettyTypeSpecifier (Nothing) = ""

prettyExType :: ExType -> String
prettyExType (ExTypeUnit) = "()"
prettyExType (ExTypeReal) = "Real"
prettyExType (ExTypeBool) = "Bool"
prettyExType (ExTypeTex tk) = "Tex " ++ show tk
prettyExType (ExTypeTuple xts) = tuple $ map prettyExType xts
prettyExType (ExTypeArray xt (ExDimFix i)) = paren (isExTypeFun xt) (prettyExType xt) ++ " " ++ show i
prettyExType (ExTypeFun xt1 xt2) = paren (isExTypeFun xt1) (prettyExType xt1) ++ " -> " ++ (prettyExType xt2)
prettyExType (ExTypeVar tv) = tv
prettyExType (ExTypeArray xt (ExDimVar dv)) = paren (isExTypeFun xt) (prettyExType xt) ++ " " ++ dv

prettySchemeDebug :: Scheme -> String
prettySchemeDebug (Scheme (settv, setdv) t) = "forall " ++ prettyBoundVarsDebug settv ++ ". forall " ++ prettyBoundVarsDebug setdv ++ ". " ++ prettyTypeDebug t

prettyBoundVarsDebug :: Show a => Set.Set a -> String
prettyBoundVarsDebug = comma . map show . Set.elems

prettyTypeDebug :: Type -> String
prettyTypeDebug (TypeUnit) = "()"
prettyTypeDebug (TypeReal) = "Real"
prettyTypeDebug (TypeBool) = "Bool"
prettyTypeDebug (TypeTex tk) = "Tex " ++ show tk
prettyTypeDebug (TypeTuple xts) = tuple $ map prettyTypeDebug xts
prettyTypeDebug (TypeArray xt (DimFix i)) = paren (isTypeFun xt) (prettyTypeDebug xt) ++ " " ++ show i
prettyTypeDebug (TypeFun xt1 xt2) = paren (isTypeFun xt1) (prettyTypeDebug xt1) ++ " -> " ++ (prettyTypeDebug xt2)
prettyTypeDebug (TypeVar tv) = show tv
prettyTypeDebug (TypeArray xt (DimVar dv)) = paren (isTypeFun xt) (prettyTypeDebug xt) ++ " " ++ show dv

-- We only need parens if the left subtype of a type is a function type.
isTypeFun :: Type -> Bool
isTypeFun (TypeFun _ _) = True
isTypeFun _ = False
isExTypeFun :: ExType -> Bool
isExTypeFun (ExTypeFun _ _) = True
isExTypeFun _ = False

prettyPatt :: Patt -> String
prettyPatt (PattWild tspec) = "_" ++ prettyTypeSpecifier tspec
prettyPatt (PattUnit tspec) = "()" ++ prettyTypeSpecifier tspec
prettyPatt (PattVar s tspec) = s ++ prettyTypeSpecifier tspec
prettyPatt (PattArray ps tspec) = array (map prettyPatt ps) ++ prettyTypeSpecifier tspec
prettyPatt (PattTuple ps tspec) = tuple (map prettyPatt ps) ++ prettyTypeSpecifier tspec

prettyExpr :: Expr -> String
prettyExpr (ExprUnitLiteral) = "()"
prettyExpr (ExprRealLiteral d) = show d
prettyExpr (ExprBoolLiteral b) = show b
prettyExpr (ExprVar s) = s
prettyExpr (ExprApp e1 e2) = paren True $ paren True (prettyExpr e1) ++ " " ++ prettyExpr e2
prettyExpr (ExprArray es) = array (map prettyExpr es)
prettyExpr (ExprTuple es) = tuple (map prettyExpr es)
prettyExpr (ExprIf ec et ef) = "if " ++ prettyExpr ec ++ " then " ++ prettyExpr et ++ " else " ++ prettyExpr ef
prettyExpr (ExprLet p ea eb) = "let " ++ prettyPatt p ++ " = " ++ prettyExpr ea ++ " in " ++ prettyExpr eb
prettyExpr (ExprLambda p e) = "\\ " ++ prettyPatt p ++ " -> " ++ prettyExpr e

prettyShaderKind :: ShaderKind -> String
prettyShaderKind ShaderKindVertex = "vertex"
prettyShaderKind ShaderKindFragment = "fragment"
