module Representation where

import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.State
import Control.Monad.Error


data ShaderKind
  = ShaderKindVertex
  | ShaderKindFragment
  
  deriving (Show, Eq)


data ShaderTextureInput
  = ShaderTextureInput1D !Int -- texture image unit
  | ShaderTextureInput2D !Int
  | ShaderTextureInput3D !Int
  | ShaderTextureInputCube !Int
  
  deriving (Show, Eq)


data ShaderState
  = ShaderState
  {
    num_uniforms :: !Int,
    num_textures :: !Int,
    num_varyings :: !Int,
    textures :: ![ShaderTextureInput],
    num_generic_outputs :: !Int, -- not including position, color etc
    num_nodes :: !Int -- node count so that new ones get unique IDs
  }
  
  deriving (Show, Eq)


data Token
  = TOK_REAL
  | TOK_BOOL
  --
  | TOK_TEXTURE1D
  | TOK_TEXTURE2D
  | TOK_TEXTURE3D
  | TOK_TEXTURECUBE
  --
  | TOK_LITERAL_BOOL !Bool
  | TOK_LITERAL_INT !Integer     -- arbitrary precision
  | TOK_LITERAL_FLOAT !Double    -- better than machine precision
  --
  | TOK_IDENTIFIER !String
  | TOK_TYPE_VAR_DIM_VAR !String
  --
  | TOK_COMMA
  | TOK_RANGE_DOTS
  | TOK_LBRACKET
  | TOK_RBRACKET
  | TOK_LPAREN
  | TOK_RPAREN
  | TOK_BACKTICK
  | TOK_WILDCARD
  --
  | TOK_OP_NOT
  | TOK_OP_SUBSCRIPT
  | TOK_OP_SWIZZLE
  | TOK_OP_SCALAR_ADD
  | TOK_OP_SCALAR_NEG_OP_SCALAR_SUB
  | TOK_OP_SCALAR_MUL
  | TOK_OP_SCALAR_DIV
  | TOK_OP_VECTOR_ADD
  | TOK_OP_VECTOR_NEG_OP_VECTOR_SUB
  | TOK_OP_VECTOR_MUL
  | TOK_OP_VECTOR_DIV
  | TOK_OP_VECTOR_SCALAR_MUL
  | TOK_OP_VECTOR_SCALAR_DIV
  | TOK_OP_MATRIX_MATRIX_LINEAR_MUL
  | TOK_OP_MATRIX_VECTOR_LINEAR_MUL
  | TOK_OP_VECTOR_MATRIX_LINEAR_MUL
  | TOK_OP_LT
  | TOK_OP_GT
  | TOK_OP_LTE
  | TOK_OP_GTE
  | TOK_OP_EQ
  | TOK_OP_NEQ
  | TOK_OP_AND
  | TOK_OP_OR
  | TOK_OP_TRANSPOSE
  | TOK_OP_APPLY
  --
  | TOK_IF
  | TOK_THEN
  | TOK_ELSE
  | TOK_LET
  | TOK_EQUALS
  | TOK_IN
  --
  | TOK_TYPESPECIFIER
  | TOK_RARROW
  | TOK_LAMBDA
  | TOK_LAMBDA_DOT
  --
  | TOK_EOF
  
  deriving (Eq, Show)


-- Rather than store identifiers for type/dim vars, we assign them numeric references.
-- These numbers are then converted back to a,b,c / m,n,o etc in the pretty print.
newtype TypeVarRef = TypeVarRef Int
  deriving (Eq, Show, Ord)

newtype DimVarRef = DimVarRef Int
  deriving (Eq, Show, Ord)

-- pools of fresh type/dim var refs, maps from identifiers to refs
type TypeEncodeContext = (([TypeVarRef], [DimVarRef]), (Map.Map String TypeVarRef, Map.Map String DimVarRef))
-- pools of fresh type/dim vars, maps from refs to identifiers
type TypeDecodeContext = (([String], [String]), (Map.Map TypeVarRef String, Map.Map DimVarRef String))

-- These variables are used in the internal representation of types.
-- We do not want to worry about alpha conversion, so they MUST NOT be reused.
-- They should only be used directly by the library initializer; after that you
-- need to ask the library which fresh variables remain.
initFreshVarRefs :: ([TypeVarRef], [DimVarRef])
initFreshVarRefs = (map TypeVarRef [0..], map DimVarRef [0..])

-- These variables are only used in converting an internal type to an external type,
-- so they can be reused as often as you like.
-- Thanks to oerjan on #haskell for this trick.
initFreshVars :: ([String], [String])
initFreshVars = (map ('\'':) $ [1..] >>= flip replicateM ['a'..'g'], map ('\'':) $ [1..] >>= flip replicateM ['m'..'z'])


-- as dims appear externally (in source or pretty-printed form)
data ExDim
  = ExDimFix !Integer
  | ExDimVar !String
  
  deriving (Show, Eq)


-- as types appear externally (in source or pretty-printed form)
data ExType
  = ExTypeUnit
  | ExTypeReal
  | ExTypeBool
  | ExTypeTexture1D
  | ExTypeTexture2D
  | ExTypeTexture3D
  | ExTypeTextureCube
  | ExTypeArray !ExType !ExDim
  | ExTypeTuple ![ExType]
  | ExTypeFun !ExType !ExType
  | ExTypeVar !String -- including the apostrophe, e.g. "'a"
  
  deriving (Show, Eq)


-- as dims are represented internally
data Dim
  = DimFix !Integer
  | DimVar !DimVarRef
  
  deriving (Show, Eq)


-- as types are represented internally
data Type
  = TypeUnit
  | TypeReal
  | TypeBool
  | TypeTexture1D
  | TypeTexture2D
  | TypeTexture3D
  | TypeTextureCube
  | TypeArray !Type !Dim
  | TypeTuple ![Type]
  | TypeFun !Type !Type
  | TypeVar !TypeVarRef
  
  deriving (Show, Eq)


-- Encodes an external type (given pools of fresh var refs).
typeFromExType :: ([TypeVarRef], [DimVarRef]) -> ExType -> (Type, ([TypeVarRef], [DimVarRef]))
typeFromExType fresh_vrefs xt =
  let (t, (fresh_vrefs', _)) = runState (typeFromExType' xt) (fresh_vrefs, (Map.empty, Map.empty)) in
    (t, fresh_vrefs')

-- Encodes many external types (in the same context).
typesFromExTypes :: ([TypeVarRef], [DimVarRef]) -> [ExType] -> ([Type], ([TypeVarRef], [DimVarRef]))
typesFromExTypes fresh_vrefs xts =
  let (t, (fresh_vrefs', _)) = runState (mapM typeFromExType' xts) (fresh_vrefs, (Map.empty, Map.empty)) in
    (t, fresh_vrefs')

typeFromExType' :: ExType -> State TypeEncodeContext Type
typeFromExType' (ExTypeUnit) = return TypeUnit
typeFromExType' (ExTypeReal) = return TypeReal
typeFromExType' (ExTypeBool) = return TypeBool
typeFromExType' (ExTypeTexture1D) = return TypeTexture1D
typeFromExType' (ExTypeTexture2D) = return TypeTexture2D
typeFromExType' (ExTypeTexture3D) = return TypeTexture3D
typeFromExType' (ExTypeTextureCube) = return TypeTextureCube
typeFromExType' (ExTypeTuple xts) = do
  ts <- mapM typeFromExType' xts
  return $ TypeTuple ts
typeFromExType' (ExTypeArray xt (ExDimFix i)) = do
  t <- typeFromExType' xt
  return $ TypeArray t (DimFix i)
typeFromExType' (ExTypeFun xt1 xt2) = do
  t1 <- typeFromExType' xt1
  t2 <- typeFromExType' xt2
  return $ TypeFun t1 t2
typeFromExType' a@(ExTypeVar tv) = do
  ((fresh_tvref:fresh_tvrefs, fresh_dvrefs), (tv_to_tvref, dv_to_dvref)) <- get
  case Map.lookup tv tv_to_tvref of
    Just tvref -> return $ TypeVar tvref
    Nothing -> do
      put ((fresh_tvrefs, fresh_dvrefs), (Map.insert tv fresh_tvref tv_to_tvref, dv_to_dvref))
      typeFromExType' a
typeFromExType' a@(ExTypeArray xt (ExDimVar dv)) = do
  t <- typeFromExType' xt
  ((fresh_tvrefs, fresh_dvref:fresh_dvrefs), (tv_to_tvref, dv_to_dvref)) <- get
  case Map.lookup dv dv_to_dvref of
    Just dvref -> return $ TypeArray t (DimVar dvref)
    Nothing -> do
      put ((fresh_tvrefs, fresh_dvrefs), (tv_to_tvref, Map.insert dv fresh_dvref dv_to_dvref))
      typeFromExType' a


-- Decodes an internal type.
exTypeFromType :: Type -> ExType
exTypeFromType t = evalState (exTypeFromType' t) (initFreshVars, (Map.empty, Map.empty))

-- Decodes many internal types (in the same context).
exTypesFromTypes :: [Type] -> [ExType]
exTypesFromTypes ts = evalState (mapM exTypeFromType' ts) (initFreshVars, (Map.empty, Map.empty))

exTypeFromType' :: Type -> State TypeDecodeContext ExType
exTypeFromType' (TypeUnit) = return ExTypeUnit
exTypeFromType' (TypeReal) = return ExTypeReal
exTypeFromType' (TypeBool) = return ExTypeBool
exTypeFromType' (TypeTexture1D) = return ExTypeTexture1D
exTypeFromType' (TypeTexture2D) = return ExTypeTexture2D
exTypeFromType' (TypeTexture3D) = return ExTypeTexture3D
exTypeFromType' (TypeTextureCube) = return ExTypeTextureCube
exTypeFromType' (TypeTuple ts) = do
  xts <- mapM exTypeFromType' ts
  return $ ExTypeTuple xts
exTypeFromType' (TypeArray t (DimFix i)) = do
  xt <- exTypeFromType' t
  return $ ExTypeArray xt (ExDimFix i)
exTypeFromType' (TypeFun t1 t2) = do
  xt1 <- exTypeFromType' t1
  xt2 <- exTypeFromType' t2
  return $ ExTypeFun xt1 xt2
exTypeFromType' a@(TypeVar tvref) = do
  ((fresh_tv:fresh_tvs, fresh_dvs), (tvref_to_tv, dvref_to_dv)) <- get
  case Map.lookup tvref tvref_to_tv of
    Just tv -> return $ ExTypeVar tv
    Nothing -> do
      put ((fresh_tvs, fresh_dvs), (Map.insert tvref fresh_tv tvref_to_tv, dvref_to_dv))
      exTypeFromType' a
exTypeFromType' a@(TypeArray t (DimVar dvref)) = do
  xt <- exTypeFromType' t
  ((fresh_tvs, fresh_dv:fresh_dvs), (tvref_to_tv, dvref_to_dv)) <- get
  case Map.lookup dvref dvref_to_dv of
    Just dv -> return $ ExTypeArray xt (ExDimVar dv)
    Nothing -> do
      put ((fresh_tvs, fresh_dvs), (tvref_to_tv, Map.insert dvref fresh_dv dvref_to_dv))
      exTypeFromType' a


data Expr
  = ExprUnitLiteral
  | ExprRealLiteral !Double
  | ExprBoolLiteral !Bool
  | ExprVar !String
  | ExprApp !Expr !Expr
  | ExprArray ![Expr]
  | ExprTuple ![Expr]
  | ExprIf !Expr !Expr !Expr
  | ExprLet !Patt !Expr !Expr -- pattern, bound expression, body expression
  | ExprLambda !Patt !Expr -- pattern, body expression
  
  deriving (Show, Eq)


data Patt
  = PattWild !(Maybe Type)
  | PattUnit !(Maybe Type)
  | PattVar !String !(Maybe Type)
  | PattArray ![Patt] !(Maybe Type)
  | PattTuple ![Patt] !(Maybe Type)
  
  deriving (Show, Eq)


data Operator
  = OpScalarNeg
  | OpVectorNeg
  | OpNot
  --
  | OpSubscript
  | OpSwizzle
  | OpScalarAdd
  | OpScalarSub
  | OpScalarMul
  | OpScalarDiv
  | OpVectorAdd
  | OpVectorSub
  | OpVectorMul
  | OpVectorDiv
  | OpVectorScalarMul
  | OpVectorScalarDiv
  | OpMatrixMatrixLinearMul
  | OpMatrixVectorLinearMul
  | OpVectorMatrixLinearMul
  | OpLessThan
  | OpGreaterThan
  | OpLessThanEqual
  | OpGreaterThanEqual
  | OpEqual
  | OpNotEqual
  | OpAnd
  | OpOr
  | OpApply
  --
  | OpTranspose
  
  deriving Eq


instance Show Operator where

  show OpScalarNeg = "negate"
  show OpVectorNeg = "negates"
  show OpNot = "~"
  --
  show OpSubscript = "!"
  show OpSwizzle = "!!"
  show OpScalarAdd = "+"
  show OpScalarSub = "-"
  show OpScalarMul = "*"
  show OpScalarDiv = "/"
  show OpVectorAdd = "++"
  show OpVectorSub = "--"
  show OpVectorMul = "**"
  show OpVectorDiv = "//"
  show OpVectorScalarMul = "**."
  show OpVectorScalarDiv = "//."
  show OpMatrixMatrixLinearMul = "#"
  show OpMatrixVectorLinearMul = "#."
  show OpVectorMatrixLinearMul = ".#"
  show OpLessThan = "<"
  show OpGreaterThan = ">"
  show OpLessThanEqual = "<="
  show OpGreaterThanEqual = ">="
  show OpEqual = "=="
  show OpNotEqual = "/="
  show OpAnd = "&&"
  show OpOr = "||"
  show OpApply = "$"
  --
  show OpTranspose = "'"


-- Dataflow graph.

data DF
  = DFReal !DFReal
  | DFBool !DFBool
  | DFSample !DFSample
  
  deriving (Show, Eq)


type DFID = Int -- unique number for node


data DFReal
  = DFRealLiteral !DFID !Double
  | DFRealVarying !DFID !Int -- the global scalar index among the varyings
  | DFRealUniform !DFID !Int -- the global scalar index among the uniforms
  --
  | DFRealCond !DFID !DFBool !DFReal !DFReal
  --
  | DFRealAdd !DFID !DFReal !DFReal
  | DFRealSub !DFID !DFReal !DFReal
  | DFRealMul !DFID !DFReal !DFReal
  | DFRealDiv !DFID !DFReal !DFReal
  | DFRealNeg !DFID !DFReal
  | DFRealRcp !DFID !DFReal
  | DFRealRsq !DFID !DFReal
  | DFRealAbs !DFID !DFReal
  | DFRealMin !DFID !DFReal !DFReal
  | DFRealMax !DFID !DFReal !DFReal
  | DFRealFloor !DFID !DFReal
  | DFRealCeiling !DFID !DFReal
  | DFRealRound !DFID !DFReal
  | DFRealTruncate !DFID !DFReal
  | DFRealFract !DFID !DFReal
  | DFRealExp !DFID !DFReal
  | DFRealExp2 !DFID !DFReal
  | DFRealLog !DFID !DFReal
  | DFRealLog2 !DFID !DFReal
  | DFRealPow !DFID !DFReal !DFReal
  | DFRealSin !DFID !DFReal
  | DFRealCos !DFID !DFReal
  | DFRealTan !DFID !DFReal
  | DFRealASin !DFID !DFReal
  | DFRealACos !DFID !DFReal
  | DFRealATan !DFID !DFReal
  --
  | DFRealGetTexR !DFID !DFSample
  | DFRealGetTexG !DFID !DFSample
  | DFRealGetTexB !DFID !DFSample
  | DFRealGetTexA !DFID !DFSample

  deriving (Show, Eq)


data DFBool
  = DFBoolLiteral !DFID !Bool
  | DFBoolVarying !DFID !Int -- the global scalar index among the varyings
  | DFBoolUniform !DFID !Int -- the global scalar index among the uniforms
  --
  | DFBoolCond !DFID !DFBool !DFBool !DFBool
  --
  | DFBoolLessThan !DFID !DFReal !DFReal
  | DFBoolLessThanEqual !DFID !DFReal !DFReal
  | DFBoolGreaterThan !DFID !DFReal !DFReal
  | DFBoolGreaterThanEqual !DFID !DFReal !DFReal
  --
  | DFBoolEqualReal !DFID !DFReal !DFReal
  | DFBoolNotEqualReal !DFID !DFReal !DFReal
  | DFBoolEqualBool !DFID !DFBool !DFBool
  | DFBoolNotEqualBool !DFID !DFBool !DFBool
  --
  | DFBoolAnd !DFID !DFBool !DFBool
  | DFBoolOr !DFID !DFBool !DFBool
  | DFBoolNot !DFID !DFBool

  deriving (Show, Eq)


data DFSample -- these are internal to a texture sampling gadget
  = DFSample1D !DFID !Int !DFReal -- texture image unit, coords
  | DFSample2D !DFID !Int !DFReal !DFReal
  | DFSample3D !DFID !Int !DFReal !DFReal !DFReal
  | DFSampleCube !DFID !Int !DFReal !DFReal !DFReal

  deriving (Show, Eq)


-- The interpreter monad holds fresh numbers,
-- and can return errors (use throwError).
type InterpretM a = ErrorT String (State ShaderState) a

runInterpretM :: InterpretM a -> ShaderState -> Either String (a, ShaderState)
runInterpretM vi i = do
  let (a,i') = runState (runErrorT vi) i
  r <- a
  return (r, i')

freshUniform :: InterpretM Int
freshUniform = do
  s <- get
  let i = num_uniforms s
  put s{num_uniforms = i+1}
  return i

freshTexture :: (Int -> ShaderTextureInput) -> InterpretM Int
freshTexture f = do
  s <- get
  let i = num_textures s
  let ts = textures s
  put s{num_textures = i+1, textures = (f i) : ts}
  return i

freshVarying :: InterpretM Int
freshVarying = do
  s <- get
  let i = num_varyings s
  put s{num_varyings = i+1}
  return i

freshGenericOutput :: InterpretM ()
freshGenericOutput = do
  s <- get
  let i = num_generic_outputs s
  put s{num_generic_outputs = i+1}
  return ()

freshNode :: InterpretM Int
freshNode = do
  s <- get
  let i = num_nodes s
  put s{num_nodes = i+1}
  return i


data Value -- can't derive Show or Eq due to those pesky closures
  = ValueUnit
  | ValueDFReal !DFReal
  | ValueDFBool !DFBool
  | ValueTexture1D !Int -- texture image unit
  | ValueTexture2D !Int
  | ValueTexture3D !Int
  | ValueTextureCube !Int
  | ValueArray ![Value]
  | ValueTuple ![Value]
  | ValueFun !(Value -> InterpretM Value) -- might raise exception

unValueDFReal :: Value -> DFReal
unValueDFReal (ValueDFReal df) = df
unValueDFReal _ = undefined

unValueDFBool :: Value -> DFBool
unValueDFBool (ValueDFBool df) = df
unValueDFBool _ = undefined

instance Show Value where

  show (ValueUnit) = "()"
  show (ValueDFReal df) = show df
  show (ValueDFBool df) = show df
  show (ValueTexture1D i) = "texture[" ++ show i ++ ", 1D]"
  show (ValueTexture2D i) = "texture[" ++ show i ++ ", 2D]"
  show (ValueTexture3D i) = "texture[" ++ show i ++ ", 3D]"
  show (ValueTextureCube i) = "texture[" ++ show i ++ ", Cube]"
  show (ValueArray vs) = "[" ++ (concat $ List.intersperse ", " $ map show vs) ++ "]"
  show (ValueTuple vs) = "(" ++ (concat $ List.intersperse ", " $ map show vs) ++ ")"
  show (ValueFun _) = "<function>"

instance Eq Value where

  (ValueUnit) == (ValueUnit) = True
  (ValueDFReal df) == (ValueDFReal df') = df == df'
  (ValueDFBool df) == (ValueDFBool df') = df == df'
  (ValueTexture1D i) == (ValueTexture1D i') = i == i'
  (ValueTexture2D i) == (ValueTexture2D i') = i == i'
  (ValueTexture3D i) == (ValueTexture3D i') = i == i'
  (ValueTextureCube i) == (ValueTextureCube i') = i == i'
  (ValueArray vs) == (ValueArray vs') = vs == vs'
  (ValueTuple vs) == (ValueTuple vs') = vs == vs'
  (ValueFun _) == (ValueFun _) = False
  _ == _ = False