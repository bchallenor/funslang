-- This module emits GLSL code for the given dataflow graph.
-- Note:
-- - Real and Bool types are both packed as floats.
-- - Uniforms are packed in a single float array, arranged as a depth first
--   search of the uniforms tuple.
-- - Varyings/attributes are packed in groups of "maxPackingSize" into
--   float vectors. If there leftover varyings, they are packed into the
--   smallest float vector/scalar that will hold all of them.

module Emit(emit) where

import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Graph
import Control.Exception

import Representation
import Dataflow



emit :: ShaderKind -> InterpretState -> DFGraph -> String
emit sk si (g, result_ns, mvn) =
  let vs = topSort g in
    unlines $
      emitGlobalDecls sk si ++
      ["","void main()", "{"] ++
      emitTempDecls mvn ++
      [""] ++
      map (\v -> case IntMap.lookup v mvn of Just n -> emitNode (sk, si) n; Nothing -> "// should have found " ++ show v) vs ++
      [""] ++
      emitCopyOut sk si result_ns ++
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

emitNameDF :: DF -> String
emitNameDF n = emitNameDFVertex (nodeID n)

emitNameDFReal :: DFReal -> String
emitNameDFReal df = emitNameDF $ DFReal df

emitNameDFBool :: DFBool -> String
emitNameDFBool df = emitNameDF $ DFBool df

emitNameDFSample :: DFSample -> String
emitNameDFSample df = emitNameDF $ DFSample df


-- Emits a function call (using emitted strings).
emitStrFun :: String -> [String] -> String
emitStrFun f args = f ++ "(" ++ (concat $ List.intersperse ", " args) ++ ")"

-- Emits an assignment (using emitted strings).
emitStrAssign :: String -> String -> String
emitStrAssign d a = d ++ " = " ++ a ++ ";"


-- Emits a unary prefix operator, assigning its result.
emitUnOpAssign :: DF -> String -> DF -> String
emitUnOpAssign d op r = emitStrAssign (emitNameDF d) (op ++ " " ++ emitNameDF r)

-- Emits a binary infix operator, assigning its result.
emitBinOpAssign :: DF -> DF -> String -> DF -> String
emitBinOpAssign d l op r = emitStrAssign (emitNameDF d) (emitNameDF l ++ " " ++ op ++ " " ++ emitNameDF r)

-- Emits a function call, assigning its result.
emitFunAssign :: DF -> String -> [DF] -> String
emitFunAssign d f args = emitStrAssign (emitNameDF d) (emitStrFun f $ map emitNameDF args)


-- Emits uniforms declaration.
emitUniformsDecl :: ShaderKind -> InterpretState -> String
emitUniformsDecl sk si =
  let n = num_uniforms si in
    if n <= 0
      then "// no uniforms"
      else "uniform float " ++ emitNameUniform sk n ++ ";"

-- Emits varying declarations (for both input and output).
emitVaryingsDecls :: ShaderKind -> InterpretState -> [String]
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
emitTextureDecls :: InterpretState -> [String]
emitTextureDecls si = map emitTextureDecl (textures si)

emitTextureDecl :: ShaderTextureInput -> String
emitTextureDecl (ShaderTextureInput1D i) = "uniform sampler1D " ++ emitNameTexture i ++ ";"
emitTextureDecl (ShaderTextureInput2D i) = "uniform sampler2D " ++ emitNameTexture i ++ ";"
emitTextureDecl (ShaderTextureInput3D i) = "uniform sampler3D " ++ emitNameTexture i ++ ";"
emitTextureDecl (ShaderTextureInputCube i) = "uniform samplerCube " ++ emitNameTexture i ++ ";"

-- Emits all relevant global declarations.
emitGlobalDecls :: ShaderKind -> InterpretState -> [String]
emitGlobalDecls sk si = emitUniformsDecl sk si : emitTextureDecls si ++ emitVaryingsDecls sk si

-- Emits temporary declarations.
emitTempDecls :: IntMap.IntMap DF -> [String]
emitTempDecls mvn =
  let (rs, bs, ss) = IntMap.foldWithKey foldTempName ([], [], []) mvn in
    [emitTempDecl "float" rs, emitTempDecl "bool" bs, emitTempDecl "vec4" ss]

foldTempName :: Vertex -> DF -> ([String], [String], [String]) -> ([String], [String], [String])
foldTempName v (DFReal _) (rs, bs, ss) = (emitNameDFVertex v : rs, bs, ss)
foldTempName v (DFBool _) (rs, bs, ss) = (rs, emitNameDFVertex v : bs, ss)
foldTempName v (DFSample _) (rs, bs, ss) = (rs, bs, emitNameDFVertex v : ss)

emitTempDecl :: String -> [String] -> String
emitTempDecl t [] = "// no " ++ t
emitTempDecl t xs = concat $ t : " " : List.intersperse ", " xs ++ [";"]


-- Emits the operation represented by a DF.
emitNode :: (ShaderKind, InterpretState) -> DF -> String

emitNode (_, _) n@(DFReal (DFRealLiteral _ d)) = emitStrAssign (emitNameDF n) $ show d
emitNode (sk, si) n@(DFReal (DFRealVarying _ i)) = emitStrAssign (emitNameDF n) $ emitNameVarying sk (num_varyings si) i
emitNode (sk, _) n@(DFReal (DFRealUniform _ i)) = emitStrAssign (emitNameDF n) $ emitNameUniform sk i

emitNode (_, _) n@(DFReal (DFRealCond _ cond p q)) = emitStrAssign (emitNameDF n) $ (emitNameDFBool cond) ++ " ? " ++ (emitNameDFReal p) ++ " : " ++ (emitNameDFReal q)

emitNode (_, _) n@(DFReal (DFRealAdd _ p q)) = emitBinOpAssign n (DFReal p) "+" (DFReal q)
emitNode (_, _) n@(DFReal (DFRealSub _ p q)) = emitBinOpAssign n (DFReal p) "-" (DFReal q)
emitNode (_, _) n@(DFReal (DFRealMul _ p q)) = emitBinOpAssign n (DFReal p) "*" (DFReal q)
emitNode (_, _) n@(DFReal (DFRealDiv _ p q)) = emitBinOpAssign n (DFReal p) "/" (DFReal q)
emitNode (_, _) n@(DFReal (DFRealNeg _ p)) = emitUnOpAssign n "-" (DFReal p)
emitNode (_, _) n@(DFReal (DFRealRcp _ p)) = emitStrAssign (emitNameDF n) $ "1 / " ++ (emitNameDFReal p)
emitNode (_, _) n@(DFReal (DFRealRsq _ p)) = emitFunAssign n "inversesqrt" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealAbs _ p)) = emitFunAssign n "abs" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealMin _ p q)) = emitFunAssign n "min" [DFReal p, DFReal q]
emitNode (_, _) n@(DFReal (DFRealMax _ p q)) = emitFunAssign n "max" [DFReal p, DFReal q]
emitNode (_, _) n@(DFReal (DFRealFloor _ p)) = emitFunAssign n "floor" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealCeiling _ p)) = emitFunAssign n "ceil" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealRound _ p)) = emitStrAssign (emitNameDF n) ("float(int(" ++ emitNameDFReal p ++ " + (" ++ emitNameDFReal p ++ " < 0 ? -0.5 : 0.5)))")
emitNode (_, _) n@(DFReal (DFRealTruncate _ p)) = emitStrAssign (emitNameDF n) ("float(int(" ++ emitNameDFReal p ++ "))")
emitNode (_, _) n@(DFReal (DFRealFract _ p)) = emitFunAssign n "fract" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealExp _ p)) = emitFunAssign n "exp" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealExp2 _ p)) = emitFunAssign n "exp2" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealLog _ p)) = emitFunAssign n "log" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealLog2 _ p)) = emitFunAssign n "log2" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealPow _ p q)) = emitFunAssign n "pow" [DFReal p, DFReal q]
emitNode (_, _) n@(DFReal (DFRealSin _ p)) = emitFunAssign n "sin" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealCos _ p)) = emitFunAssign n "cos" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealTan _ p)) = emitFunAssign n "tan" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealASin _ p)) = emitFunAssign n "asin" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealACos _ p)) = emitFunAssign n "acos" [DFReal p]
emitNode (_, _) n@(DFReal (DFRealATan _ p)) = emitFunAssign n "atan" [DFReal p]

emitNode (_, _) n@(DFReal (DFRealGetTexR _ p)) = emitStrAssign (emitNameDF n) (emitNameDFSample p ++ ".r")
emitNode (_, _) n@(DFReal (DFRealGetTexG _ p)) = emitStrAssign (emitNameDF n) (emitNameDFSample p ++ ".g")
emitNode (_, _) n@(DFReal (DFRealGetTexB _ p)) = emitStrAssign (emitNameDF n) (emitNameDFSample p ++ ".b")
emitNode (_, _) n@(DFReal (DFRealGetTexA _ p)) = emitStrAssign (emitNameDF n) (emitNameDFSample p ++ ".a")

emitNode (_, _) n@(DFBool (DFBoolLiteral _ b)) = emitStrAssign (emitNameDF n) $ show b
emitNode (sk, si) n@(DFBool (DFBoolVarying _ i)) = emitStrAssign (emitNameDF n) $ "bool(" ++ emitNameVarying sk (num_varyings si) i ++ ")"
emitNode (sk, _) n@(DFBool (DFBoolUniform _ i)) = emitStrAssign (emitNameDF n) $ "bool(" ++ emitNameUniform sk i ++ ")"

emitNode (_, _) n@(DFBool (DFBoolCond _ cond p q)) = emitStrAssign (emitNameDF n) $ (emitNameDFBool cond) ++ " ? " ++ (emitNameDFBool p) ++ " : " ++ (emitNameDFBool q)

emitNode (_, _) n@(DFBool (DFBoolLessThan _ p q)) = emitBinOpAssign n (DFReal p) "<" (DFReal q)
emitNode (_, _) n@(DFBool (DFBoolLessThanEqual _ p q)) = emitBinOpAssign n (DFReal p) "<=" (DFReal q)
emitNode (_, _) n@(DFBool (DFBoolGreaterThan _ p q)) = emitBinOpAssign n (DFReal p) ">" (DFReal q)
emitNode (_, _) n@(DFBool (DFBoolGreaterThanEqual _ p q)) = emitBinOpAssign n (DFReal p) ">=" (DFReal q)

emitNode (_, _) n@(DFBool (DFBoolEqualReal _ p q)) = emitBinOpAssign n (DFReal p) "==" (DFReal q)
emitNode (_, _) n@(DFBool (DFBoolNotEqualReal _ p q)) = emitBinOpAssign n (DFReal p) "!=" (DFReal q)
emitNode (_, _) n@(DFBool (DFBoolEqualBool _ p q)) = emitBinOpAssign n (DFBool p) "==" (DFBool q)
emitNode (_, _) n@(DFBool (DFBoolNotEqualBool _ p q)) = emitBinOpAssign n (DFBool p) "!=" (DFBool q)

emitNode (_, _) n@(DFBool (DFBoolAnd _ p q)) = emitBinOpAssign n (DFBool p) "&&" (DFBool q)
emitNode (_, _) n@(DFBool (DFBoolOr _ p q)) = emitBinOpAssign n (DFBool p) "||" (DFBool q)
emitNode (_, _) n@(DFBool (DFBoolNot _ p)) = emitUnOpAssign n "!" (DFBool p)

emitNode (_, _) n@(DFSample (DFSample1D _ i p)) = emitStrAssign (emitNameDF n) (emitStrFun "texture1D" [emitNameTexture i, emitNameDFReal p])
emitNode (_, _) n@(DFSample (DFSample2D _ i p q)) = emitStrAssign (emitNameDF n) (emitStrFun "texture2D" [emitNameTexture i, "vec2(" ++ emitNameDFReal p ++ ", " ++ emitNameDFReal q ++ ")"]) 
emitNode (_, _) n@(DFSample (DFSample3D _ i p q r)) = emitStrAssign (emitNameDF n) (emitStrFun "texture3D" [emitNameTexture i, "vec3(" ++ emitNameDFReal p ++ ", " ++ emitNameDFReal q ++ ", " ++ emitNameDFReal r ++ ")"]) 
emitNode (_, _) n@(DFSample (DFSampleCube _ i p q r)) = emitStrAssign (emitNameDF n) (emitStrFun "textureCube" [emitNameTexture i, "vec3(" ++ emitNameDFReal p ++ ", " ++ emitNameDFReal q ++ ", " ++ emitNameDFReal r ++ ")"]) 


-- Emits copy out code to save results.
emitCopyOut :: ShaderKind -> InterpretState -> [DF] -> [String]
emitCopyOut ShaderKindVertex si (x:y:z:w : output_varyings) =
  (emitStrAssign "gl_Position" $ "vec4(" ++ emitNameDF x ++ ", " ++ emitNameDF y ++ ", " ++ emitNameDF z ++ ", " ++ emitNameDF w ++ ")") :
  zipWith (\n i -> emitStrAssign (emitNameVarying ShaderKindFragment (num_generic_outputs si) i) (emitNameDF n)) output_varyings [0..]
emitCopyOut ShaderKindVertex _ _ = undefined
emitCopyOut ShaderKindFragment _ (r:g:b:a:[]) =
  (emitStrAssign "gl_FragColor" $ "vec4(" ++ emitNameDF r ++ ", " ++ emitNameDF g ++ ", " ++ emitNameDF b ++ ", " ++ emitNameDF a ++ ")") :
  []
emitCopyOut ShaderKindFragment si (cond:rest) =
  ("if (!" ++ emitNameDF cond ++ ") discard;") :
  emitCopyOut ShaderKindFragment si rest
emitCopyOut ShaderKindFragment _ _ = undefined
