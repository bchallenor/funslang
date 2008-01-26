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



emit :: ShaderKind -> ShaderInputOutput -> DFGraph -> String
emit sk si (g, result_ns, mnv, mvn) =
  let vs = topSort g in
    unlines $
      emitDecls sk si ++
      ["","void main()", "{"] ++
      map (\v -> let Just n = Map.lookup v mvn in emitNode (sk, si, mnv) n) vs ++
      [""] ++
      emitCopyOut sk si mnv result_ns ++
      ["}"]


-- The names that GLSL gives to Funslang varyings.
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

emitRootNameUniform :: ShaderKind -> String
emitRootNameUniform ShaderKindVertex = "VertexUniforms"
emitRootNameUniform ShaderKindFragment = "FragmentUniforms"

emitNameUniform :: ShaderKind -> Int -> String
emitNameUniform sk i = emitRootNameUniform sk ++ "[" ++ show i ++ "]"

emitRootNameVarying :: ShaderKind -> String
emitRootNameVarying ShaderKindVertex = "VertexVaryings"
emitRootNameVarying ShaderKindFragment = "FragmentVaryings"

emitNameVarying :: ShaderKind -> Int -> Int -> String
emitNameVarying sk num_total i =
  let (d, m) = i `divMod` maxPackingSize in
    emitRootNameVarying sk ++ show (maxPackingSize * d) ++
      if i == (num_total - 1) && m == 0
        then "" -- no need to subscript: it's the last element, and that element is packed into a float
        else "[" ++ show m ++ "]"

emitNameTexture :: Int -> String
emitNameTexture i = "Tex" ++ show i

emitNameDFVertex :: Vertex -> String
emitNameDFVertex v = "t" ++ show v

emitNameDF :: Map.Map DF Vertex -> DF -> String
emitNameDF mnv n = case Map.lookup n mnv of
  Just v -> emitNameDFVertex v
  Nothing -> error "unknown vertex"

emitNameDFReal :: Map.Map DF Vertex -> DFReal -> String
emitNameDFReal mnv df = emitNameDF mnv $ DFReal df

emitNameDFBool :: Map.Map DF Vertex -> DFBool -> String
emitNameDFBool mnv df = emitNameDF mnv $ DFBool df

emitNameDFSample :: Map.Map DF Vertex -> DFSample -> String
emitNameDFSample mnv df = emitNameDF mnv $ DFSample df


-- Emits a function call (using emitted strings).
emitStrFun :: String -> [String] -> String
emitStrFun f args = f ++ "(" ++ (concat $ List.intersperse ", " args) ++ ")"

-- Emits an assignment (using emitted strings).
emitStrAssign :: String -> String -> String
emitStrAssign d a = d ++ " = " ++ a ++ ";"


-- Emits a unary prefix operator, assigning its result.
emitUnOpAssign :: Map.Map DF Vertex -> DF -> String -> DF -> String
emitUnOpAssign mnv d op r = emitStrAssign (emitNameDF mnv d) (op ++ " " ++ emitNameDF mnv r)

-- Emits a binary infix operator, assigning its result.
emitBinOpAssign :: Map.Map DF Vertex -> DF -> DF -> String -> DF -> String
emitBinOpAssign mnv d l op r = emitStrAssign (emitNameDF mnv d) (emitNameDF mnv l ++ " " ++ op ++ " " ++ emitNameDF mnv r)

-- Emits a function call, assigning its result.
emitFunAssign :: Map.Map DF Vertex -> DF -> String -> [DF] -> String
emitFunAssign mnv d f args = emitStrAssign (emitNameDF mnv d) (emitStrFun f $ map (emitNameDF mnv) args)


-- Emits uniforms declaration.
emitUniformsDecl :: ShaderKind -> ShaderInputOutput -> String
emitUniformsDecl sk si = "uniform float " ++ emitNameUniform sk (num_uniforms si) ++ ";"

-- Emits varying declarations (for both input and output).
emitVaryingsDecls :: ShaderKind -> ShaderInputOutput -> [String]
emitVaryingsDecls ShaderKindVertex si = emitVaryingsDecls' ShaderKindVertex (num_varyings si) ++ emitVaryingsDecls' ShaderKindFragment (num_generic_outputs si)
emitVaryingsDecls ShaderKindFragment si = emitVaryingsDecls' ShaderKindFragment (num_varyings si)

emitVaryingsDecls' :: ShaderKind -> Int -> [String]
emitVaryingsDecls' sk num_total = emitVaryingsDecls'' sk num_total 0 []

emitVaryingsDecls'' :: ShaderKind -> Int -> Int -> [String] -> [String]
emitVaryingsDecls'' sk num_total num_packed acc =
  let num_left = num_total - num_packed in
    if num_left <= 0
      then acc
      else
        let num_now = min num_left maxPackingSize in
        let decl = emitVaryingQualifier sk ++ " " ++ emitPackingType num_now ++ " " ++ emitRootNameVarying sk ++ show num_packed ++ ";" in
          emitVaryingsDecls'' sk num_total (num_packed + num_now) (decl : acc)

-- Emits texture declarations.
emitTextureDecls :: ShaderInputOutput -> [String]
emitTextureDecls si = map emitTextureDecl (textures si)

emitTextureDecl :: ShaderTextureInput -> String
emitTextureDecl (ShaderTextureInput1D i) = "uniform sampler1D " ++ emitNameTexture i ++ ";"
emitTextureDecl (ShaderTextureInput2D i) = "uniform sampler2D " ++ emitNameTexture i ++ ";"
emitTextureDecl (ShaderTextureInput3D i) = "uniform sampler3D " ++ emitNameTexture i ++ ";"
emitTextureDecl (ShaderTextureInputCube i) = "uniform samplerCube " ++ emitNameTexture i ++ ";"

-- Emits all relevant declarations.
emitDecls :: ShaderKind -> ShaderInputOutput -> [String]
emitDecls sk si = emitUniformsDecl sk si : emitTextureDecls si ++ emitVaryingsDecls sk si


-- Emits the operation represented by a DF.
emitNode :: (ShaderKind, ShaderInputOutput, Map.Map DF Vertex) -> DF -> String

emitNode (_, _, mnv) n@(DFReal (DFRealLiteral d)) = emitStrAssign (emitNameDF mnv n) $ show d
emitNode (sk, si, mnv) n@(DFReal (DFRealVarying i)) = emitStrAssign (emitNameDF mnv n) $ emitNameVarying sk (num_varyings si) i
emitNode (sk, _, mnv) n@(DFReal (DFRealUniform i)) = emitStrAssign (emitNameDF mnv n) $ emitNameUniform sk i

emitNode (_, _, mnv) n@(DFReal (DFRealCond cond p q)) = emitStrAssign (emitNameDF mnv n) $ (emitNameDFBool mnv cond) ++ " ? " ++ (emitNameDFReal mnv p) ++ " : " ++ (emitNameDFReal mnv q)

emitNode (_, _, mnv) n@(DFReal (DFRealAdd p q)) = emitBinOpAssign mnv n (DFReal p) "+" (DFReal q)
emitNode (_, _, mnv) n@(DFReal (DFRealSub p q)) = emitBinOpAssign mnv n (DFReal p) "-" (DFReal q)
emitNode (_, _, mnv) n@(DFReal (DFRealMul p q)) = emitBinOpAssign mnv n (DFReal p) "*" (DFReal q)
emitNode (_, _, mnv) n@(DFReal (DFRealDiv p q)) = emitBinOpAssign mnv n (DFReal p) "/" (DFReal q)
emitNode (_, _, mnv) n@(DFReal (DFRealNeg p)) = emitUnOpAssign mnv n "-" (DFReal p)
emitNode (_, _, mnv) n@(DFReal (DFRealRcp p)) = emitStrAssign (emitNameDF mnv n) $ "1 / " ++ (emitNameDFReal mnv p)
emitNode (_, _, mnv) n@(DFReal (DFRealRsq p)) = emitFunAssign mnv n "inversesqrt" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealAbs p)) = emitFunAssign mnv n "abs" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealMin p q)) = emitFunAssign mnv n "min" [DFReal p, DFReal q]
emitNode (_, _, mnv) n@(DFReal (DFRealMax p q)) = emitFunAssign mnv n "max" [DFReal p, DFReal q]
emitNode (_, _, mnv) n@(DFReal (DFRealFloor p)) = emitFunAssign mnv n "floor" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealCeiling p)) = emitFunAssign mnv n "ceil" [DFReal p]
emitNode (_, _, mnv) n@(DFReal (DFRealRound p)) = emitStrAssign (emitNameDF mnv n) ("float(int(" ++ emitNameDFReal mnv p ++ " + (" ++ emitNameDFReal mnv p ++ " < 0 ? -0.5 : 0.5)))")
emitNode (_, _, mnv) n@(DFReal (DFRealTruncate p)) = emitStrAssign (emitNameDF mnv n) ("float(int(" ++ emitNameDFReal mnv p ++ "))")
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

emitNode (_, _, mnv) n@(DFReal (DFRealGetTexR p)) = emitStrAssign (emitNameDF mnv n) (emitNameDFSample mnv p ++ ".r")
emitNode (_, _, mnv) n@(DFReal (DFRealGetTexG p)) = emitStrAssign (emitNameDF mnv n) (emitNameDFSample mnv p ++ ".g")
emitNode (_, _, mnv) n@(DFReal (DFRealGetTexB p)) = emitStrAssign (emitNameDF mnv n) (emitNameDFSample mnv p ++ ".b")
emitNode (_, _, mnv) n@(DFReal (DFRealGetTexA p)) = emitStrAssign (emitNameDF mnv n) (emitNameDFSample mnv p ++ ".a")

emitNode (_, _, mnv) n@(DFBool (DFBoolLiteral b)) = emitStrAssign (emitNameDF mnv n) $ show b
emitNode (sk, si, mnv) n@(DFBool (DFBoolVarying i)) = emitStrAssign (emitNameDF mnv n) $ "bool(" ++ emitNameVarying sk (num_varyings si) i ++ ")"
emitNode (sk, _, mnv) n@(DFBool (DFBoolUniform i)) = emitStrAssign (emitNameDF mnv n) $ "bool(" ++ emitNameUniform sk i ++ ")"

emitNode (_, _, mnv) n@(DFBool (DFBoolCond cond p q)) = emitStrAssign (emitNameDF mnv n) $ (emitNameDFBool mnv cond) ++ " ? " ++ (emitNameDFBool mnv p) ++ " : " ++ (emitNameDFBool mnv q)

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

emitNode (_, _, mnv) n@(DFSample (DFSample1D i p)) = emitStrAssign (emitNameDF mnv n) (emitStrFun "texture1D" [emitNameTexture i, emitNameDFReal mnv p])
emitNode (_, _, mnv) n@(DFSample (DFSample2D i p q)) = emitStrAssign (emitNameDF mnv n) (emitStrFun "texture2D" [emitNameTexture i, "vec2(" ++ emitNameDFReal mnv p ++ ", " ++ emitNameDFReal mnv q ++ ")"]) 
emitNode (_, _, mnv) n@(DFSample (DFSample3D i p q r)) = emitStrAssign (emitNameDF mnv n) (emitStrFun "texture3D" [emitNameTexture i, "vec3(" ++ emitNameDFReal mnv p ++ ", " ++ emitNameDFReal mnv q ++ ", " ++ emitNameDFReal mnv r ++ ")"]) 
emitNode (_, _, mnv) n@(DFSample (DFSampleCube i p q r)) = emitStrAssign (emitNameDF mnv n) (emitStrFun "textureCube" [emitNameTexture i, "vec3(" ++ emitNameDFReal mnv p ++ ", " ++ emitNameDFReal mnv q ++ ", " ++ emitNameDFReal mnv r ++ ")"]) 


-- Emits copy out code to save results.
emitCopyOut :: ShaderKind -> ShaderInputOutput -> Map.Map DF Vertex -> [DF] -> [String]
emitCopyOut ShaderKindVertex si mnv (x:y:z:w : output_varyings) =
  (emitStrAssign "gl_Position" $ "vec4(" ++ emitNameDF mnv x ++ ", " ++ emitNameDF mnv y ++ ", " ++ emitNameDF mnv z ++ ", " ++ emitNameDF mnv w ++ ")") :
  zipWith (\n i -> emitStrAssign (emitNameVarying ShaderKindFragment (num_generic_outputs si) i) (emitNameDF mnv n)) output_varyings [0..]
emitCopyOut ShaderKindVertex _ _ _ = undefined
emitCopyOut ShaderKindFragment _ mnv (r:g:b:a:[]) =
  (emitStrAssign "gl_FragColor" $ "vec4(" ++ emitNameDF mnv r ++ ", " ++ emitNameDF mnv g ++ ", " ++ emitNameDF mnv b ++ ", " ++ emitNameDF mnv a ++ ")") :
  []
emitCopyOut ShaderKindFragment si mnv (cond:rest) =
  ("if (!" ++ emitNameDF mnv cond ++ ") discard;") :
  emitCopyOut ShaderKindFragment si mnv rest
emitCopyOut ShaderKindFragment _ _ _ = undefined
