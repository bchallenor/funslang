module Representation where

import qualified Data.Map as Map
import Control.Monad.State


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
  | TOK_TYPE_VAR !String
  --
  | TOK_COMMA
  | TOK_RANGE_DOTS
  | TOK_LBRACKET
  | TOK_RBRACKET
  | TOK_LPAREN
  | TOK_RPAREN
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
  
  deriving (Eq, Show)


-- Rather than store identifiers for type/dim vars, we assign them numeric references.
-- These numbers are then converted back to a,b,c / m,n,o etc in the pretty print.
data TypeVarRef = TypeVarRef !Int
  deriving (Eq, Show, Ord)

data DimVarRef = DimVarRef !Int
  deriving (Eq, Show, Ord)

-- pool of fresh var refs, map from identifiers to refs
type TypeEncodeContext = (([TypeVarRef], Map.Map String TypeVarRef), ([DimVarRef], Map.Map String DimVarRef))
-- pool of fresh vars, map from refs to identifiers
type TypeDecodeContext = (([String], Map.Map TypeVarRef String), ([String], Map.Map DimVarRef String))

-- pools to draw from
typeVarRefs :: [TypeVarRef]
typeVarRefs = map TypeVarRef [0..]
typeVars :: [String]
typeVars = map (\x->['\'',x]) ['a'..'l']
dimVarRefs :: [DimVarRef]
dimVarRefs = map DimVarRef [0..]
dimVars :: [String]
dimVars = map (\x->[x]) ['m'..'z']

-- as types appear in source or pretty-printed form
data DecodedType
  = UnitDecodedType
  | RealDecodedType
  | BoolDecodedType
  | Texture1DDecodedType
  | Texture2DDecodedType
  | Texture3DDecodedType
  | TextureCubeDecodedType
  | ArrayDecodedType !DecodedType !Integer
  | TupleDecodedType ![DecodedType]
  | FunDecodedType !DecodedType !DecodedType
  | TypeVarDecodedType !String -- including the apostrophe, e.g. "'a"
  | DimVarDecodedType !DecodedType !String
  
  deriving (Show, Eq)


-- as types are represented internally
data Type
  = UnitType
  | RealType
  | BoolType
  | Texture1DType
  | Texture2DType
  | Texture3DType
  | TextureCubeType
  | ArrayType !Type !Integer
  | TupleType ![Type]
  | FunType !Type !Type
  | TypeVarType !TypeVarRef
  | DimVarType !Type !DimVarRef
  
  deriving (Show, Eq)


-- Takes pools of fresh var refs and a source-form type, and encodes it.
encodeType :: [TypeVarRef] -> [DimVarRef] -> DecodedType -> Type
encodeType fresh_tvrefs fresh_dvrefs dt = evalState (encodeType' dt) ((fresh_tvrefs, Map.empty), (fresh_dvrefs, Map.empty))

encodeType' :: DecodedType -> State TypeEncodeContext Type
encodeType' (UnitDecodedType) = return UnitType
encodeType' (RealDecodedType) = return RealType
encodeType' (BoolDecodedType) = return BoolType
encodeType' (Texture1DDecodedType) = return Texture1DType
encodeType' (Texture2DDecodedType) = return Texture2DType
encodeType' (Texture3DDecodedType) = return Texture3DType
encodeType' (TextureCubeDecodedType) = return TextureCubeType
encodeType' (TupleDecodedType dts) = do
  ts <- mapM encodeType' dts
  return $ TupleType ts
encodeType' (ArrayDecodedType dt i) = do
  t <- encodeType' dt
  return $ ArrayType t i
encodeType' (FunDecodedType dt1 dt2) = do
  t1 <- encodeType' dt1
  t2 <- encodeType' dt2
  return $ FunType t1 t2
encodeType' (TypeVarDecodedType tv) = do
  ((fresh_tvrefs, tv_to_tvref), (fresh_dvrefs, dv_to_dvref)) <- get
  case Map.lookup tv tv_to_tvref of
    Just tvref -> return $ TypeVarType tvref
    Nothing -> do
      put ((tail fresh_tvrefs, Map.insert tv (head fresh_tvrefs) tv_to_tvref), (fresh_dvrefs, dv_to_dvref))
      encodeType' $ TypeVarDecodedType tv
encodeType' (DimVarDecodedType dt dv) = do
  t <- encodeType' dt
  ((fresh_tvrefs, tv_to_tvref), (fresh_dvrefs, dv_to_dvref)) <- get
  case Map.lookup dv dv_to_dvref of
    Just dvref -> return $ DimVarType t dvref
    Nothing -> do
      put ((fresh_tvrefs, tv_to_tvref), (tail fresh_dvrefs, Map.insert dv (head fresh_dvrefs) dv_to_dvref))
      encodeType' $ DimVarDecodedType dt dv


-- Takes an internal type, and decodes it.
decodeType :: Type -> DecodedType
decodeType t = evalState (decodeType' t) ((typeVars, Map.empty), (dimVars, Map.empty))

decodeType' :: Type -> State TypeDecodeContext DecodedType
decodeType' (UnitType) = return UnitDecodedType
decodeType' (RealType) = return RealDecodedType
decodeType' (BoolType) = return BoolDecodedType
decodeType' (Texture1DType) = return Texture1DDecodedType
decodeType' (Texture2DType) = return Texture2DDecodedType
decodeType' (Texture3DType) = return Texture3DDecodedType
decodeType' (TextureCubeType) = return TextureCubeDecodedType
decodeType' (TupleType ts) = do
  dts <- mapM decodeType' ts
  return $ TupleDecodedType dts
decodeType' (ArrayType t i) = do
  dt <- decodeType' t
  return $ ArrayDecodedType dt i
decodeType' (FunType t1 t2) = do
  dt1 <- decodeType' t1
  dt2 <- decodeType' t2
  return $ FunDecodedType dt1 dt2
decodeType' (TypeVarType tvref) = do
  ((fresh_tvs, tvref_to_tv), (fresh_dvs, dvref_to_dv)) <- get
  case Map.lookup tvref tvref_to_tv of
    Just tv -> return $ TypeVarDecodedType tv
    Nothing -> do
      put ((tail fresh_tvs, Map.insert tvref (head fresh_tvs) tvref_to_tv), (fresh_dvs, dvref_to_dv))
      decodeType' $ TypeVarType tvref
decodeType' (DimVarType t dvref) = do
  dt <- decodeType' t
  ((fresh_tvs, tvref_to_tv), (fresh_dvs, dvref_to_dv)) <- get
  case Map.lookup dvref dvref_to_dv of
    Just dv -> return $ DimVarDecodedType dt dv
    Nothing -> do
      put ((fresh_tvs, tvref_to_tv), (tail fresh_dvs, Map.insert dvref (head fresh_dvs) dvref_to_dv))
      decodeType' $ DimVarType t dvref


data Expr
  = UnitConstExpr
  | RealConstExpr !Double
  | BoolConstExpr !Bool
  | VarExpr !String
  | AppExpr !Expr !Expr
  | ArrayExpr ![Expr]
  | TupleExpr ![Expr]
  | IfExpr !Expr !Expr !Expr
  | LetExpr !Patt !Expr !Expr -- pattern, bound expression, body expression
  | LambdaExpr !Patt !Expr -- pattern, body expression
  
  deriving (Show, Eq)


data Patt
  = WildPatt
  | UnitPatt
  | VarPatt !String
  | ArrayPatt ![Patt]
  | TuplePatt ![Patt]
  
  deriving (Show, Eq)


data Operator
  = OpScalarNeg
  | OpVectorNeg
  | OpNot
  ---
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
  ---
  | OpTranspose
  
  deriving Eq


instance Show Operator where
  show OpScalarNeg = "negate"
  show OpVectorNeg = "negates"
  show OpNot = "~"
  ---
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
  ---
  show OpTranspose = "'"
