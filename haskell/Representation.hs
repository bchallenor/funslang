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
typeFromExType :: [TypeVarRef] -> [DimVarRef] -> ExType -> Type
typeFromExType fresh_tvrefs fresh_dvrefs xt = evalState (typeFromExType' xt) ((fresh_tvrefs, Map.empty), (fresh_dvrefs, Map.empty))

-- Encodes many external types (in the same context).
typesFromExTypes :: [TypeVarRef] -> [DimVarRef] -> [ExType] -> [Type]
typesFromExTypes fresh_tvrefs fresh_dvrefs xts = evalState (mapM typeFromExType' xts) ((fresh_tvrefs, Map.empty), (fresh_dvrefs, Map.empty))

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
  ((fresh_tvrefs, tv_to_tvref), (fresh_dvrefs, dv_to_dvref)) <- get
  case Map.lookup tv tv_to_tvref of
    Just tvref -> return $ TypeVar tvref
    Nothing -> do
      put ((tail fresh_tvrefs, Map.insert tv (head fresh_tvrefs) tv_to_tvref), (fresh_dvrefs, dv_to_dvref))
      typeFromExType' a
typeFromExType' a@(ExTypeArray xt (ExDimVar dv)) = do
  t <- typeFromExType' xt
  ((fresh_tvrefs, tv_to_tvref), (fresh_dvrefs, dv_to_dvref)) <- get
  case Map.lookup dv dv_to_dvref of
    Just dvref -> return $ TypeArray t (DimVar dvref)
    Nothing -> do
      put ((fresh_tvrefs, tv_to_tvref), (tail fresh_dvrefs, Map.insert dv (head fresh_dvrefs) dv_to_dvref))
      typeFromExType' a


-- Decodes an internal type.
exTypeFromType :: Type -> ExType
exTypeFromType t = evalState (exTypeFromType' t) ((typeVars, Map.empty), (dimVars, Map.empty))

-- Decodes many internal types (in the same context).
exTypesFromTypes :: [Type] -> [ExType]
exTypesFromTypes ts = evalState (mapM exTypeFromType' ts) ((typeVars, Map.empty), (dimVars, Map.empty))

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
  ((fresh_tvs, tvref_to_tv), (fresh_dvs, dvref_to_dv)) <- get
  case Map.lookup tvref tvref_to_tv of
    Just tv -> return $ ExTypeVar tv
    Nothing -> do
      put ((tail fresh_tvs, Map.insert tvref (head fresh_tvs) tvref_to_tv), (fresh_dvs, dvref_to_dv))
      exTypeFromType' a
exTypeFromType' a@(TypeArray t (DimVar dvref)) = do
  xt <- exTypeFromType' t
  ((fresh_tvs, tvref_to_tv), (fresh_dvs, dvref_to_dv)) <- get
  case Map.lookup dvref dvref_to_dv of
    Just dv -> return $ ExTypeArray xt (ExDimVar dv)
    Nothing -> do
      put ((fresh_tvs, tvref_to_tv), (tail fresh_dvs, Map.insert dvref (head fresh_dvs) dvref_to_dv))
      exTypeFromType' a


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
