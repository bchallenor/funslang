module Representation where

import qualified Data.Map as Map

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
-- These numbers are then converted back to a,b,c etc in the pretty print.
class (Eq a, Show a, Ord a) => VarRef a where
  nextFresh :: a -> a

data TypeVarRef = TypeVarRef !Int
  deriving (Eq, Show, Ord)

instance VarRef TypeVarRef where
  nextFresh (TypeVarRef i) = TypeVarRef (succ i)

data DimVarRef = DimVarRef !Int
  deriving (Eq, Show, Ord)

instance VarRef DimVarRef where
  nextFresh (DimVarRef i) = DimVarRef (succ i)

-- next unmapped ref, map from identifiers to refs
type VarRefEncodeContext = ((TypeVarRef, Map.Map Char TypeVarRef), (DimVarRef, Map.Map Char DimVarRef))
-- next unmapped var, map from refs to identifiers
type VarRefDecodeContext = ((Char, Map.Map TypeVarRef Char), (Char, Map.Map DimVarRef Char))


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
