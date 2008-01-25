module Emit where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Graph

import Representation
import Dataflow




emit :: DFGraph -> String
emit (g, mnv, mvn) =
  let vs = topSort g in
    unlines $ map (\v -> let Just n = Map.lookup v mvn in emitNode mnv n) vs


-- Locations are temporary variables where dataflow node results are stored.
-- These emit the given location.

emitLocVertex :: Vertex -> String
emitLocVertex v = "t" ++ show v

emitLocDF :: Map.Map DF Vertex -> DF -> String
emitLocDF mnv n = case Map.lookup n mnv of
  Just v -> emitLocVertex v
  Nothing -> error "unknown vertex"

emitLocDFReal :: Map.Map DF Vertex -> DFReal -> String
emitLocDFReal mnv df = emitLocDF mnv $ DFReal df

emitLocDFBool :: Map.Map DF Vertex -> DFBool -> String
emitLocDFBool mnv df = emitLocDF mnv $ DFBool df

emitLocDFSample :: Map.Map DF Vertex -> DFSample -> String
emitLocDFSample mnv df = emitLocDF mnv $ DFSample df


-- Emits a function call (using emitted strings).
emitStrFun :: String -> [String] -> String
emitStrFun f args = f ++ "(" ++ (concat $ List.intersperse ", " args) ++ ")"

-- Emits an assignment (using emitted strings).
emitStrAssign :: String -> String -> String
emitStrAssign d a = d ++ " = " ++ a ++ ";"


-- Emits a unary prefix operator, assigning its result.
emitUnOpAssign :: Map.Map DF Vertex -> DF -> String -> DF -> String
emitUnOpAssign mnv d op r = emitStrAssign (emitLocDF mnv d) (op ++ " " ++ emitLocDF mnv r)

-- Emits a binary infix operator, assigning its result.
emitBinOpAssign :: Map.Map DF Vertex -> DF -> DF -> String -> DF -> String
emitBinOpAssign mnv d l op r = emitStrAssign (emitLocDF mnv d) (emitLocDF mnv l ++ " " ++ op ++ " " ++ emitLocDF mnv r)

-- Emits a function call, assigning its result.
emitFunAssign :: Map.Map DF Vertex -> DF -> String -> [DF] -> String
emitFunAssign mnv d f args = emitStrAssign (emitLocDF mnv d) (emitStrFun f $ map (emitLocDF mnv) args)


-- Emits the operation represented by a DF.
emitNode :: Map.Map DF Vertex -> DF -> String

emitNode mnv n@(DFReal (DFRealLiteral d)) = emitStrAssign (emitLocDF mnv n) $ show d
emitNode mnv n@(DFReal (DFRealVarying i)) = emitStrAssign (emitLocDF mnv n) $ "RealVarying[" ++ show i ++ "]"
emitNode mnv n@(DFReal (DFRealUniform i)) = emitStrAssign (emitLocDF mnv n) $ "RealUniform[" ++ show i ++ "]"

emitNode mnv n@(DFReal (DFRealCond cond p q)) = emitStrAssign (emitLocDF mnv n) $ (emitLocDFBool mnv cond) ++ " ? " ++ (emitLocDFReal mnv p) ++ " : " ++ (emitLocDFReal mnv q)

emitNode mnv n@(DFReal (DFRealAdd p q)) = emitBinOpAssign mnv n (DFReal p) "+" (DFReal q)
emitNode mnv n@(DFReal (DFRealSub p q)) = emitBinOpAssign mnv n (DFReal p) "-" (DFReal q)
emitNode mnv n@(DFReal (DFRealMul p q)) = emitBinOpAssign mnv n (DFReal p) "*" (DFReal q)
emitNode mnv n@(DFReal (DFRealDiv p q)) = emitBinOpAssign mnv n (DFReal p) "/" (DFReal q)
emitNode mnv n@(DFReal (DFRealNeg p)) = emitUnOpAssign mnv n "-" (DFReal p)
emitNode mnv n@(DFReal (DFRealRcp p)) = emitStrAssign (emitLocDF mnv n) $ "1 / " ++ (emitLocDFReal mnv p)
emitNode mnv n@(DFReal (DFRealRsq p)) = emitFunAssign mnv n "inversesqrt" [DFReal p]
emitNode mnv n@(DFReal (DFRealAbs p)) = emitFunAssign mnv n "abs" [DFReal p]
emitNode mnv n@(DFReal (DFRealMin p q)) = emitFunAssign mnv n "min" [DFReal p, DFReal q]
emitNode mnv n@(DFReal (DFRealMax p q)) = emitFunAssign mnv n "max" [DFReal p, DFReal q]
emitNode mnv n@(DFReal (DFRealFloor p)) = emitFunAssign mnv n "floor" [DFReal p]
emitNode mnv n@(DFReal (DFRealCeiling p)) = emitFunAssign mnv n "ceil" [DFReal p]
emitNode mnv n@(DFReal (DFRealRound p)) = emitStrAssign (emitLocDF mnv n) ("float(int(" ++ emitLocDFReal mnv p ++ " + (" ++ emitLocDFReal mnv p ++ " < 0 ? -0.5 : 0.5)))")
emitNode mnv n@(DFReal (DFRealTruncate p)) = emitStrAssign (emitLocDF mnv n) ("float(int(" ++ emitLocDFReal mnv p ++ "))")
emitNode mnv n@(DFReal (DFRealFract p)) = emitFunAssign mnv n "fract" [DFReal p]
emitNode mnv n@(DFReal (DFRealExp p)) = emitFunAssign mnv n "exp" [DFReal p]
emitNode mnv n@(DFReal (DFRealExp2 p)) = emitFunAssign mnv n "exp2" [DFReal p]
emitNode mnv n@(DFReal (DFRealLog p)) = emitFunAssign mnv n "log" [DFReal p]
emitNode mnv n@(DFReal (DFRealLog2 p)) = emitFunAssign mnv n "log2" [DFReal p]
emitNode mnv n@(DFReal (DFRealPow p q)) = emitFunAssign mnv n "pow" [DFReal p, DFReal q]
emitNode mnv n@(DFReal (DFRealSin p)) = emitFunAssign mnv n "sin" [DFReal p]
emitNode mnv n@(DFReal (DFRealCos p)) = emitFunAssign mnv n "cos" [DFReal p]
emitNode mnv n@(DFReal (DFRealTan p)) = emitFunAssign mnv n "tan" [DFReal p]
emitNode mnv n@(DFReal (DFRealASin p)) = emitFunAssign mnv n "asin" [DFReal p]
emitNode mnv n@(DFReal (DFRealACos p)) = emitFunAssign mnv n "acos" [DFReal p]
emitNode mnv n@(DFReal (DFRealATan p)) = emitFunAssign mnv n "atan" [DFReal p]

emitNode mnv n@(DFReal (DFRealGetTexR p)) = emitStrAssign (emitLocDF mnv n) (emitLocDFSample mnv p ++ ".r")
emitNode mnv n@(DFReal (DFRealGetTexG p)) = emitStrAssign (emitLocDF mnv n) (emitLocDFSample mnv p ++ ".g")
emitNode mnv n@(DFReal (DFRealGetTexB p)) = emitStrAssign (emitLocDF mnv n) (emitLocDFSample mnv p ++ ".b")
emitNode mnv n@(DFReal (DFRealGetTexA p)) = emitStrAssign (emitLocDF mnv n) (emitLocDFSample mnv p ++ ".a")

emitNode mnv n@(DFBool (DFBoolLiteral b)) = emitStrAssign (emitLocDF mnv n) $ show b
emitNode mnv n@(DFBool (DFBoolVarying i)) = emitStrAssign (emitLocDF mnv n) $ "BoolVarying[" ++ show i ++ "]"
emitNode mnv n@(DFBool (DFBoolUniform i)) = emitStrAssign (emitLocDF mnv n) $ "BoolUniform[" ++ show i ++ "]"

emitNode mnv n@(DFBool (DFBoolCond cond p q)) = emitStrAssign (emitLocDF mnv n) $ (emitLocDFBool mnv cond) ++ " ? " ++ (emitLocDFBool mnv p) ++ " : " ++ (emitLocDFBool mnv q)

emitNode mnv n@(DFBool (DFBoolLessThan p q)) = emitBinOpAssign mnv n (DFReal p) "<" (DFReal q)
emitNode mnv n@(DFBool (DFBoolLessThanEqual p q)) = emitBinOpAssign mnv n (DFReal p) "<=" (DFReal q)
emitNode mnv n@(DFBool (DFBoolGreaterThan p q)) = emitBinOpAssign mnv n (DFReal p) ">" (DFReal q)
emitNode mnv n@(DFBool (DFBoolGreaterThanEqual p q)) = emitBinOpAssign mnv n (DFReal p) ">=" (DFReal q)

emitNode mnv n@(DFBool (DFBoolEqualReal p q)) = emitBinOpAssign mnv n (DFReal p) "==" (DFReal q)
emitNode mnv n@(DFBool (DFBoolNotEqualReal p q)) = emitBinOpAssign mnv n (DFReal p) "!=" (DFReal q)
emitNode mnv n@(DFBool (DFBoolEqualBool p q)) = emitBinOpAssign mnv n (DFBool p) "==" (DFBool q)
emitNode mnv n@(DFBool (DFBoolNotEqualBool p q)) = emitBinOpAssign mnv n (DFBool p) "!=" (DFBool q)

emitNode mnv n@(DFBool (DFBoolAnd p q)) = emitBinOpAssign mnv n (DFBool p) "&&" (DFBool q)
emitNode mnv n@(DFBool (DFBoolOr p q)) = emitBinOpAssign mnv n (DFBool p) "||" (DFBool q)
emitNode mnv n@(DFBool (DFBoolNot p)) = emitUnOpAssign mnv n "!" (DFBool p)

emitNode mnv n@(DFSample (DFSample1D i p)) = emitStrAssign (emitLocDF mnv n) (emitStrFun "texture1D" ["Tex" ++ show i, emitLocDFReal mnv p])
emitNode mnv n@(DFSample (DFSample2D i p q)) = emitStrAssign (emitLocDF mnv n) (emitStrFun "texture2D" ["Tex" ++ show i, "vec2(" ++ emitLocDFReal mnv p ++ ", " ++ emitLocDFReal mnv q ++ ")"]) 
emitNode mnv n@(DFSample (DFSample3D i p q r)) = emitStrAssign (emitLocDF mnv n) (emitStrFun "texture3D" ["Tex" ++ show i, "vec3(" ++ emitLocDFReal mnv p ++ ", " ++ emitLocDFReal mnv q ++ ", " ++ emitLocDFReal mnv r ++ ")"]) 
emitNode mnv n@(DFSample (DFSampleCube i p q r)) = emitStrAssign (emitLocDF mnv n) (emitStrFun "textureCube" ["Tex" ++ show i, "vec3(" ++ emitLocDFReal mnv p ++ ", " ++ emitLocDFReal mnv q ++ ", " ++ emitLocDFReal mnv r ++ ")"]) 
