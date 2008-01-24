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

-- Emits an assignment.
emitAssign :: String -> String -> String
emitAssign d a = d ++ " = " ++ a ++ ";"

-- Emits an operator.
emitOp :: String -> String -> String -> String
emitOp l op r = l ++ " " ++ op ++ " " ++ r

-- Emits a function call.
emitFun :: String -> [String] -> String
emitFun f args = f ++ "(" ++ (concat $ List.intersperse ", " args) ++ ")"

-- Emits the operation represented by a DF.
emitNode :: Map.Map DF Int -> DF -> String

emitNode mnv n@(DFReal (DFRealLiteral d)) = emitAssign (emitLocDF mnv n) (show d)
emitNode mnv n@(DFReal (DFRealVarying i)) = emitAssign (emitLocDF mnv n) $ "RealVarying[" ++ show i ++ "]"
emitNode mnv n@(DFReal (DFRealUniform i)) = emitAssign (emitLocDF mnv n) $ "RealUniform[" ++ show i ++ "]"

emitNode mnv n@(DFReal (DFRealCond cond p q)) = emitAssign (emitLocDF mnv n) $ (emitLocDFBool mnv cond) ++ " ? " ++ (emitLocDFReal mnv p) ++ " : " ++ (emitLocDFReal mnv q)

emitNode mnv n@(DFReal (DFRealAdd p q)) = emitAssign (emitLocDF mnv n) (emitOp (emitLocDFReal mnv p) "+" (emitLocDFReal mnv q))
emitNode mnv n@(DFReal (DFRealSub p q)) = emitAssign (emitLocDF mnv n) (emitOp (emitLocDFReal mnv p) "-" (emitLocDFReal mnv q))
emitNode mnv n@(DFReal (DFRealMul p q)) = emitAssign (emitLocDF mnv n) (emitOp (emitLocDFReal mnv p) "*" (emitLocDFReal mnv q))
emitNode mnv n@(DFReal (DFRealDiv p q)) = emitAssign (emitLocDF mnv n) (emitOp (emitLocDFReal mnv p) "/" (emitLocDFReal mnv q))
emitNode mnv n@(DFReal (DFRealNeg p)) = emitAssign (emitLocDF mnv n) ("-" ++ (emitLocDFReal mnv p))
-- emitNode mnv n@(DFReal (DFRealRcp p)) = "Rcp"
emitNode mnv n@(DFReal (DFRealRsq p)) = emitAssign (emitLocDF mnv n) (emitFun "inversesqrt" [emitLocDFReal mnv p])
-- emitNode mnv n@(DFReal (DFRealAbs p)) = "Abs"
-- emitNode mnv n@(DFReal (DFRealMin p q)) = "Min"
-- emitNode mnv n@(DFReal (DFRealMax p q)) = "Max"
-- emitNode mnv n@(DFReal (DFRealFloor p)) = "Floor"
-- emitNode mnv n@(DFReal (DFRealCeiling p)) = "Ceiling"
-- emitNode mnv n@(DFReal (DFRealRound p)) = "Round"
-- emitNode mnv n@(DFReal (DFRealTruncate p)) = "Truncate"
-- emitNode mnv n@(DFReal (DFRealFract p)) = "Fract"
-- emitNode mnv n@(DFReal (DFRealExp p)) = "Exp"
-- emitNode mnv n@(DFReal (DFRealExp2 p)) = "Exp2"
-- emitNode mnv n@(DFReal (DFRealLog p)) = "Log"
-- emitNode mnv n@(DFReal (DFRealLog2 p)) = "Log2"
-- emitNode mnv n@(DFReal (DFRealPow p q)) = "Pow"
-- emitNode mnv n@(DFReal (DFRealSin p)) = "Sin"
-- emitNode mnv n@(DFReal (DFRealCos p)) = "Cos"
-- emitNode mnv n@(DFReal (DFRealTan p)) = "Tan"
-- emitNode mnv n@(DFReal (DFRealASin p)) = "ASin"
-- emitNode mnv n@(DFReal (DFRealACos p)) = "ACos"
-- emitNode mnv n@(DFReal (DFRealATan p)) = "ATan"

emitNode mnv n@(DFReal (DFRealGetTexR p)) = emitAssign (emitLocDF mnv n) ((emitLocDFSample mnv p) ++ ".r")
emitNode mnv n@(DFReal (DFRealGetTexG p)) = emitAssign (emitLocDF mnv n) ((emitLocDFSample mnv p) ++ ".g")
emitNode mnv n@(DFReal (DFRealGetTexB p)) = emitAssign (emitLocDF mnv n) ((emitLocDFSample mnv p) ++ ".b")
emitNode mnv n@(DFReal (DFRealGetTexA p)) = emitAssign (emitLocDF mnv n) ((emitLocDFSample mnv p) ++ ".a")

emitNode mnv n@(DFBool (DFBoolLiteral b)) = emitAssign (emitLocDF mnv n) (show b)
emitNode mnv n@(DFBool (DFBoolVarying i)) = emitAssign (emitLocDF mnv n) $ "BoolVarying[" ++ show i ++ "]"
emitNode mnv n@(DFBool (DFBoolUniform i)) = emitAssign (emitLocDF mnv n) $ "BoolUniform[" ++ show i ++ "]"

-- emitNode mnv n@(DFBool (DFBoolCond cond p q)) = "BoolCond"

-- emitNode mnv n@(DFBool (DFBoolLessThan p q)) = "LessThan"
-- emitNode mnv n@(DFBool (DFBoolLessThanEqual p q)) = "LessThanEqual"
-- emitNode mnv n@(DFBool (DFBoolGreaterThan p q)) = "GreaterThan"
-- emitNode mnv n@(DFBool (DFBoolGreaterThanEqual p q)) = "GreaterThanEqual"

-- emitNode mnv n@(DFBool (DFBoolEqualReal p q)) = "EqualReal"
-- emitNode mnv n@(DFBool (DFBoolNotEqualReal p q)) = "NotEqualReal"
-- emitNode mnv n@(DFBool (DFBoolEqualBool p q)) = "EqualBool"
-- emitNode mnv n@(DFBool (DFBoolNotEqualBool p q)) = "NotEqualBool"

-- emitNode mnv n@(DFBool (DFBoolAnd p q)) = "And"
-- emitNode mnv n@(DFBool (DFBoolOr p q)) = "Or"
-- emitNode mnv n@(DFBool (DFBoolNot p)) = "Not"

emitNode mnv n@(DFSample (DFSample1D i p)) = emitAssign (emitLocDF mnv n) (emitFun "texture1D" ["Tex" ++ show i, emitLocDFReal mnv p])
emitNode mnv n@(DFSample (DFSample2D i p q)) = emitAssign (emitLocDF mnv n) (emitFun "texture2D" ["Tex" ++ show i, "vec2(" ++ emitLocDFReal mnv p ++ ", " ++ emitLocDFReal mnv q ++ ")"]) 
emitNode mnv n@(DFSample (DFSample3D i p q r)) = emitAssign (emitLocDF mnv n) (emitFun "texture3D" ["Tex" ++ show i, "vec3(" ++ emitLocDFReal mnv p ++ ", " ++ emitLocDFReal mnv q ++ ", " ++ emitLocDFReal mnv r ++ ")"]) 
emitNode mnv n@(DFSample (DFSampleCube i p q r)) = emitAssign (emitLocDF mnv n) (emitFun "textureCube" ["Tex" ++ show i, "vec3(" ++ emitLocDFReal mnv p ++ ", " ++ emitLocDFReal mnv q ++ ", " ++ emitLocDFReal mnv r ++ ")"]) 
