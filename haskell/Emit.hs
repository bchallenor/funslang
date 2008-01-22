module Emit where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Array
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

emitLocDFNode :: Map.Map DFNode Vertex -> DFNode -> String
emitLocDFNode mnv n = case Map.lookup n mnv of
  Just v -> emitLocVertex v
  Nothing -> error "unknown vertex"

emitLocDFReal :: Map.Map DFNode Vertex -> DFReal -> String
emitLocDFReal mnv df = emitLocDFNode mnv $ DFNodeReal df

emitLocDFBool :: Map.Map DFNode Vertex -> DFBool -> String
emitLocDFBool mnv df = emitLocDFNode mnv $ DFNodeBool df

emitLocDFSample :: Map.Map DFNode Vertex -> DFSample -> String
emitLocDFSample mnv df = emitLocDFNode mnv $ DFNodeSample df

-- Emits an assignment.
emitAssign :: String -> String -> String
emitAssign d a = d ++ " = " ++ a ++ ";"

-- Emits an operator.
emitOp :: String -> String -> String -> String
emitOp l op r = l ++ " " ++ op ++ " " ++ r

-- Emits a function call.
emitFun :: String -> [String] -> String
emitFun f args = f ++ "(" ++ (concat $ List.intersperse ", " args) ++ ")"

-- Emits the operation represented by a DFNode.
emitNode :: Map.Map DFNode Int -> DFNode -> String

emitNode mnv n@(DFNodeReal (DFRealLiteral d)) = emitAssign (emitLocDFNode mnv n) (show d)
emitNode mnv n@(DFNodeReal (DFRealVarying i)) = emitAssign (emitLocDFNode mnv n) $ "RealVarying[" ++ show i ++ "]"
emitNode mnv n@(DFNodeReal (DFRealUniform i)) = emitAssign (emitLocDFNode mnv n) $ "RealUniform[" ++ show i ++ "]"

emitNode mnv n@(DFNodeReal (DFRealCond cond p q)) = emitAssign (emitLocDFNode mnv n) $ (emitLocDFBool mnv cond) ++ " ? " ++ (emitLocDFReal mnv p) ++ " : " ++ (emitLocDFReal mnv q)

emitNode mnv n@(DFNodeReal (DFRealAdd p q)) = emitAssign (emitLocDFNode mnv n) (emitOp (emitLocDFReal mnv p) "+" (emitLocDFReal mnv q))
emitNode mnv n@(DFNodeReal (DFRealSub p q)) = emitAssign (emitLocDFNode mnv n) (emitOp (emitLocDFReal mnv p) "-" (emitLocDFReal mnv q))
emitNode mnv n@(DFNodeReal (DFRealMul p q)) = emitAssign (emitLocDFNode mnv n) (emitOp (emitLocDFReal mnv p) "*" (emitLocDFReal mnv q))
emitNode mnv n@(DFNodeReal (DFRealDiv p q)) = emitAssign (emitLocDFNode mnv n) (emitOp (emitLocDFReal mnv p) "/" (emitLocDFReal mnv q))
emitNode mnv n@(DFNodeReal (DFRealNeg p)) = emitAssign (emitLocDFNode mnv n) ("-" ++ (emitLocDFReal mnv p))
-- emitNode mnv n@(DFNodeReal (DFRealRcp p)) = "Rcp"
emitNode mnv n@(DFNodeReal (DFRealRsq p)) = emitAssign (emitLocDFNode mnv n) (emitFun "inversesqrt" [emitLocDFReal mnv p])
-- emitNode mnv n@(DFNodeReal (DFRealAbs p)) = "Abs"
-- emitNode mnv n@(DFNodeReal (DFRealMin p q)) = "Min"
-- emitNode mnv n@(DFNodeReal (DFRealMax p q)) = "Max"
-- emitNode mnv n@(DFNodeReal (DFRealFloor p)) = "Floor"
-- emitNode mnv n@(DFNodeReal (DFRealCeiling p)) = "Ceiling"
-- emitNode mnv n@(DFNodeReal (DFRealRound p)) = "Round"
-- emitNode mnv n@(DFNodeReal (DFRealTruncate p)) = "Truncate"
-- emitNode mnv n@(DFNodeReal (DFRealFract p)) = "Fract"
-- emitNode mnv n@(DFNodeReal (DFRealExp p)) = "Exp"
-- emitNode mnv n@(DFNodeReal (DFRealExp2 p)) = "Exp2"
-- emitNode mnv n@(DFNodeReal (DFRealLog p)) = "Log"
-- emitNode mnv n@(DFNodeReal (DFRealLog2 p)) = "Log2"
-- emitNode mnv n@(DFNodeReal (DFRealPow p q)) = "Pow"
-- emitNode mnv n@(DFNodeReal (DFRealSin p)) = "Sin"
-- emitNode mnv n@(DFNodeReal (DFRealCos p)) = "Cos"
-- emitNode mnv n@(DFNodeReal (DFRealTan p)) = "Tan"
-- emitNode mnv n@(DFNodeReal (DFRealASin p)) = "ASin"
-- emitNode mnv n@(DFNodeReal (DFRealACos p)) = "ACos"
-- emitNode mnv n@(DFNodeReal (DFRealATan p)) = "ATan"

emitNode mnv n@(DFNodeReal (DFRealGetTexR p)) = emitAssign (emitLocDFNode mnv n) ((emitLocDFSample mnv p) ++ ".r")
emitNode mnv n@(DFNodeReal (DFRealGetTexG p)) = emitAssign (emitLocDFNode mnv n) ((emitLocDFSample mnv p) ++ ".g")
emitNode mnv n@(DFNodeReal (DFRealGetTexB p)) = emitAssign (emitLocDFNode mnv n) ((emitLocDFSample mnv p) ++ ".b")
emitNode mnv n@(DFNodeReal (DFRealGetTexA p)) = emitAssign (emitLocDFNode mnv n) ((emitLocDFSample mnv p) ++ ".a")

emitNode mnv n@(DFNodeBool (DFBoolLiteral b)) = emitAssign (emitLocDFNode mnv n) (show b)
emitNode mnv n@(DFNodeBool (DFBoolVarying i)) = emitAssign (emitLocDFNode mnv n) $ "BoolVarying[" ++ show i ++ "]"
emitNode mnv n@(DFNodeBool (DFBoolUniform i)) = emitAssign (emitLocDFNode mnv n) $ "BoolUniform[" ++ show i ++ "]"

-- emitNode mnv n@(DFNodeBool (DFBoolCond cond p q)) = "BoolCond"

-- emitNode mnv n@(DFNodeBool (DFBoolLessThan p q)) = "LessThan"
-- emitNode mnv n@(DFNodeBool (DFBoolLessThanEqual p q)) = "LessThanEqual"
-- emitNode mnv n@(DFNodeBool (DFBoolGreaterThan p q)) = "GreaterThan"
-- emitNode mnv n@(DFNodeBool (DFBoolGreaterThanEqual p q)) = "GreaterThanEqual"

-- emitNode mnv n@(DFNodeBool (DFBoolEqualReal p q)) = "EqualReal"
-- emitNode mnv n@(DFNodeBool (DFBoolNotEqualReal p q)) = "NotEqualReal"
-- emitNode mnv n@(DFNodeBool (DFBoolEqualBool p q)) = "EqualBool"
-- emitNode mnv n@(DFNodeBool (DFBoolNotEqualBool p q)) = "NotEqualBool"

-- emitNode mnv n@(DFNodeBool (DFBoolAnd p q)) = "And"
-- emitNode mnv n@(DFNodeBool (DFBoolOr p q)) = "Or"
-- emitNode mnv n@(DFNodeBool (DFBoolNot p)) = "Not"

emitNode mnv n@(DFNodeSample (DFSample1D i p)) = emitAssign (emitLocDFNode mnv n) (emitFun "texture1D" ["Tex" ++ show i, emitLocDFReal mnv p])
emitNode mnv n@(DFNodeSample (DFSample2D i p q)) = emitAssign (emitLocDFNode mnv n) (emitFun "texture2D" ["Tex" ++ show i, "vec2(" ++ emitLocDFReal mnv p ++ ", " ++ emitLocDFReal mnv q ++ ")"]) 
emitNode mnv n@(DFNodeSample (DFSample3D i p q r)) = emitAssign (emitLocDFNode mnv n) (emitFun "texture3D" ["Tex" ++ show i, "vec3(" ++ emitLocDFReal mnv p ++ ", " ++ emitLocDFReal mnv q ++ ", " ++ emitLocDFReal mnv r ++ ")"]) 
emitNode mnv n@(DFNodeSample (DFSampleCube i p q r)) = emitAssign (emitLocDFNode mnv n) (emitFun "textureCube" ["Tex" ++ show i, "vec3(" ++ emitLocDFReal mnv p ++ ", " ++ emitLocDFReal mnv q ++ ", " ++ emitLocDFReal mnv r ++ ")"]) 
