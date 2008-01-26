-- This module emits GLSL code for the given dataflow graph.
-- Note:
-- - Real and Bool types are both packed as floats.
-- - Uniforms are packed in a single float array, arranged as a depth first
--   search of the uniforms tuple.
-- - Varyings/attributes are packed in groups of "maxPackingSize" into
--   float vectors. If there leftover varyings, they are packed into the
--   smallest float vector/scalar that will hold all of them.

module Emit(emit) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Graph
import Control.Exception

import Representation
import Dataflow



emit :: ShaderKind -> ShaderInputs -> DFGraph -> String
emit sk si (g, mnv, mvn) =
  let vs = topSort g in
    unlines $
      emitDecls sk si ++
      map (\v -> let Just n = Map.lookup v mvn in emitNode (sk, si, mnv) n) vs



emitVaryingQualifier :: ShaderKind -> String
emitVaryingQualifier ShaderKindVertex = "attribute"
emitVaryingQualifier ShaderKindFragment = "varying"

-- The size of the largest vector available for packing vertex attributes.
maxPackingSize :: Int
maxPackingSize = 4

-- Emits a vector type with n components.
emitPackingType :: Int -> String
emitPackingType 1 = "float"
emitPackingType n = assert (n > 0 && n <= maxPackingSize) "vec" ++ show n


-- These emit variable names.

emitRootLocUniform :: ShaderKind -> String
emitRootLocUniform ShaderKindVertex = "VertexUniforms"
emitRootLocUniform ShaderKindFragment = "FragmentUniforms"

emitLocUniform :: ShaderKind -> Int -> String
emitLocUniform sk i = emitRootLocUniform sk ++ "[" ++ show i ++ "]"

emitRootLocVarying :: ShaderKind -> String
emitRootLocVarying ShaderKindVertex = "VertexVaryings"
emitRootLocVarying ShaderKindFragment = "FragmentVaryings"

emitLocVarying :: ShaderKind -> ShaderInputs -> Int -> String
emitLocVarying sk si i =
  let (d, m) = i `divMod` maxPackingSize in
    emitRootLocVarying sk ++ show (maxPackingSize * d) ++
      if i == (num_varyings si - 1) && m == 0
        then "" -- no need to subscript: it's the last element, and that element is packed into a float
        else "[" ++ show m ++ "]"

emitLocTexture :: Int -> String
emitLocTexture i = "Tex" ++ show i

emitLocDFVertex :: Vertex -> String
emitLocDFVertex v = "t" ++ show v

emitLocDF :: Map.Map DF Vertex -> DF -> String
emitLocDF mnv n = case Map.lookup n mnv of
  Just v -> emitLocDFVertex v
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


-- Emits uniforms declaration.
emitUniformsDecl :: ShaderKind -> ShaderInputs -> String
emitUniformsDecl sk si = "uniform float " ++ emitLocUniform sk (num_uniforms si) ++ ";"

-- Emits varyings declarations.
emitVaryingsDecls :: ShaderKind -> ShaderInputs -> [String]
emitVaryingsDecls sk si = emitVaryingsDecls' sk si 0 []

emitVaryingsDecls' :: ShaderKind -> ShaderInputs -> Int -> [String] -> [String]
emitVaryingsDecls' sk si num_packed acc =
  let num_left = num_varyings si - num_packed in
    if num_left <= 0
      then acc
      else
        let num_now = min num_left maxPackingSize in
        let decl = emitVaryingQualifier sk ++ " " ++ emitPackingType num_now ++ " " ++ emitRootLocVarying sk ++ show num_packed ++ ";" in
          emitVaryingsDecls' sk si (num_packed + num_now) (decl : acc)

-- Emits texture declarations.
emitTextureDecls :: ShaderInputs -> [String]
emitTextureDecls si = map emitTextureDecl (textures si)

emitTextureDecl :: ShaderTextureInput -> String
emitTextureDecl (ShaderTextureInput1D i) = "uniform sampler1D " ++ emitLocTexture i ++ ";"
emitTextureDecl (ShaderTextureInput2D i) = "uniform sampler2D " ++ emitLocTexture i ++ ";"
emitTextureDecl (ShaderTextureInput3D i) = "uniform sampler3D " ++ emitLocTexture i ++ ";"
emitTextureDecl (ShaderTextureInputCube i) = "uniform samplerCube " ++ emitLocTexture i ++ ";"

-- Emits all relevant declarations.
emitDecls :: ShaderKind -> ShaderInputs -> [String]
emitDecls sk si = emitUniformsDecl sk si : emitTextureDecls si ++ emitVaryingsDecls sk si


-- Emits the operation represented by a DF.
emitNode :: (ShaderKind, ShaderInputs, Map.Map DF Vertex) -> DF -> String

emitNode (_, _, mnv) n@(DFReal (DFRealLiteral d)) = emitStrAssign (emitLocDF mnv n) $ show d
emitNode (sk, si, mnv) n@(DFReal (DFRealVarying i)) = emitStrAssign (emitLocDF mnv n) $ emitLocVarying sk si i
emitNode (sk, _, mnv) n@(DFReal (DFRealUniform i)) = emitStrAssign (emitLocDF mnv n) $ emitLocUniform sk i

emitNode (_, _, mnv) n@(DFReal (DFRealCond cond p q)) = emitStrAssign (emitLocDF mnv n) $ (emitLocDFBool mnv cond) ++ " ? " ++ (emitLocDFReal mnv p) ++ " : " ++ (emitLocDFReal mnv q)

emitNode (_, _, mnv) n@(DFReal (DFRealAdd p q)) = emitBinOpAssign mnv n (DFReal p) "+" (DFReal q)
emitNode (_, _, mnv) n@(DFReal (DFRealSub p q)) = emitBinOpAssign mnv n (DFReal p) "-" (DFReal q)
emitNode (_, _, mnv) n@(DFReal (DFRealMul p q)) = emitBinOpAssign mnv n (DFReal p) "*" (DFReal q)
emitNode (_, _, mnv) n@(DFReal (DFRealDiv p q)) = emitBinOpAssign mnv n (DFReal p) "/" (DFReal q)
emitNode (_, _, mnv) n@(DFReal (DFRealNeg p)) = emitUnOpAssign mnv n "-" (DFReal p)
emitNode (_, _, mnv) n@(DFReal (DFRealRcp p)) = emitStrAssign (emitLocDF mnv n) $ "1 / " ++ (emitLocDFReal mnv p)
emitNode (_, _, mnv) n@(DFReal (DFRealRsq p)) = emitFunAssign mnv n "inversesqrt" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealAbs p)) = emitFunAssign mnv n "abs" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealMin p q)) = emitFunAssign mnv n "min" [DFReal p, DFReal q]
emitNode (_, _, mnv) n@(DFReal (DFRealMax p q)) = emitFunAssign mnv n "max" [DFReal p, DFReal q]
emitNode (_, _, mnv) n@(DFReal (DFRealFloor p)) = emitFunAssign mnv n "floor" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealCeiling p)) = emitFunAssign mnv n "ceil" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealRound p)) = emitStrAssign (emitLocDF mnv n) ("float(int(" ++ emitLocDFReal mnv p ++ " + (" ++ emitLocDFReal mnv p ++ " < 0 ? -0.5 : 0.5)))")
emitNode (_, _, mnv) n@(DFReal (DFRealTruncate p)) = emitStrAssign (emitLocDF mnv n) ("float(int(" ++ emitLocDFReal mnv p ++ "))")
emitNode (_, _, mnv) n@(DFReal (DFRealFract p)) = emitFunAssign mnv n "fract" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealExp p)) = emitFunAssign mnv n "exp" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealExp2 p)) = emitFunAssign mnv n "exp2" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealLog p)) = emitFunAssign mnv n "log" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealLog2 p)) = emitFunAssign mnv n "log2" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealPow p q)) = emitFunAssign mnv n "pow" [DFReal p, DFReal q]
emitNode (_, _, mnv) n@(DFReal (DFRealSin p)) = emitFunAssign mnv n "sin" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealCos p)) = emitFunAssign mnv n "cos" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealTan p)) = emitFunAssign mnv n "tan" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealASin p)) = emitFunAssign mnv n "asin" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealACos p)) = emitFunAssign mnv n "acos" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealATan p)) = emitFunAssign mnv n "atan" [DFReal p]

emitNode (_, _, mnv) n@(DFReal (DFRealGetTexR p)) = emitStrAssign (emitLocDF mnv n) (emitLocDFSample mnv p ++ ".r")
emitNode (_, _, mnv) n@(DFReal (DFRealGetTexG p)) = emitStrAssign (emitLocDF mnv n) (emitLocDFSample mnv p ++ ".g")
emitNode (_, _, mnv) n@(DFReal (DFRealGetTexB p)) = emitStrAssign (emitLocDF mnv n) (emitLocDFSample mnv p ++ ".b")
emitNode (_, _, mnv) n@(DFReal (DFRealGetTexA p)) = emitStrAssign (emitLocDF mnv n) (emitLocDFSample mnv p ++ ".a")

emitNode (_, _, mnv) n@(DFBool (DFBoolLiteral b)) = emitStrAssign (emitLocDF mnv n) $ show b
emitNode (sk, si, mnv) n@(DFBool (DFBoolVarying i)) = emitStrAssign (emitLocDF mnv n) $ "bool(" ++ emitLocVarying sk si i ++ ")"
emitNode (sk, _, mnv) n@(DFBool (DFBoolUniform i)) = emitStrAssign (emitLocDF mnv n) $ "bool(" ++ emitLocUniform sk i ++ ")"

emitNode (_, _, mnv) n@(DFBool (DFBoolCond cond p q)) = emitStrAssign (emitLocDF mnv n) $ (emitLocDFBool mnv cond) ++ " ? " ++ (emitLocDFBool mnv p) ++ " : " ++ (emitLocDFBool mnv q)

emitNode (_, _, mnv) n@(DFBool (DFBoolLessThan p q)) = emitBinOpAssign mnv n (DFReal p) "<" (DFReal q)
emitNode (_, _, mnv) n@(DFBool (DFBoolLessThanEqual p q)) = emitBinOpAssign mnv n (DFReal p) "<=" (DFReal q)
emitNode (_, _, mnv) n@(DFBool (DFBoolGreaterThan p q)) = emitBinOpAssign mnv n (DFReal p) ">" (DFReal q)
emitNode (_, _, mnv) n@(DFBool (DFBoolGreaterThanEqual p q)) = emitBinOpAssign mnv n (DFReal p) ">=" (DFReal q)

emitNode (_, _, mnv) n@(DFBool (DFBoolEqualReal p q)) = emitBinOpAssign mnv n (DFReal p) "==" (DFReal q)
emitNode (_, _, mnv) n@(DFBool (DFBoolNotEqualReal p q)) = emitBinOpAssign mnv n (DFReal p) "!=" (DFReal q)
emitNode (_, _, mnv) n@(DFBool (DFBoolEqualBool p q)) = emitBinOpAssign mnv n (DFBool p) "==" (DFBool q)
emitNode (_, _, mnv) n@(DFBool (DFBoolNotEqualBool p q)) = emitBinOpAssign mnv n (DFBool p) "!=" (DFBool q)

emitNode (_, _, mnv) n@(DFBool (DFBoolAnd p q)) = emitBinOpAssign mnv n (DFBool p) "&&" (DFBool q)
emitNode (_, _, mnv) n@(DFBool (DFBoolOr p q)) = emitBinOpAssign mnv n (DFBool p) "||" (DFBool q)
emitNode (_, _, mnv) n@(DFBool (DFBoolNot p)) = emitUnOpAssign mnv n "!" (DFBool p)

emitNode (_, _, mnv) n@(DFSample (DFSample1D i p)) = emitStrAssign (emitLocDF mnv n) (emitStrFun "texture1D" [emitLocTexture i, emitLocDFReal mnv p])
emitNode (_, _, mnv) n@(DFSample (DFSample2D i p q)) = emitStrAssign (emitLocDF mnv n) (emitStrFun "texture2D" [emitLocTexture i, "vec2(" ++ emitLocDFReal mnv p ++ ", " ++ emitLocDFReal mnv q ++ ")"]) 
emitNode (_, _, mnv) n@(DFSample (DFSample3D i p q r)) = emitStrAssign (emitLocDF mnv n) (emitStrFun "texture3D" [emitLocTexture i, "vec3(" ++ emitLocDFReal mnv p ++ ", " ++ emitLocDFReal mnv q ++ ", " ++ emitLocDFReal mnv r ++ ")"]) 
emitNode (_, _, mnv) n@(DFSample (DFSampleCube i p q r)) = emitStrAssign (emitLocDF mnv n) (emitStrFun "textureCube" [emitLocTexture i, "vec3(" ++ emitLocDFReal mnv p ++ ", " ++ emitLocDFReal mnv q ++ ", " ++ emitLocDFReal mnv r ++ ")"]) 
