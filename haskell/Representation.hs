module Representation where

import Library


data Token
  = TOK_BOOL
  | TOK_INT
  | TOK_FLOAT
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
  | TOK_OP_SUBSCRIPT
  | TOK_OP_SWIZZLE
  | TOK_OP_APPEND
  | TOK_OP_TRANSPOSE
  | TOK_OP_NOT
  | TOK_OP_MUL
  | TOK_OP_DIV
  | TOK_OP_LINEAR_MUL
  | TOK_OP_SCALE_MUL
  | TOK_OP_SCALE_DIV
  | TOK_OP_ADD
  | TOK_OP_NEG_OP_SUB
  | TOK_OP_LT
  | TOK_OP_GT
  | TOK_OP_LTE
  | TOK_OP_GTE
  | TOK_OP_EQ
  | TOK_OP_NEQ
  | TOK_OP_AND
  | TOK_OP_OR
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


data Type
  = UnitType
  | IntType
  | FloatType
  | BoolType
  | Texture1DType
  | Texture2DType
  | Texture3DType
  | TextureCubeType
  | TupleType ![Type]
  | ArrayType !Type !Integer
  | FunType !Type !Type
  
  deriving (Show, Eq)


data Expr
  = UnitExpr
  | IntExpr !Integer
  | FloatExpr !Double
  | BoolExpr !Bool
  | VarExpr !String
  | AppOp1Expr !Operator1 !Expr
  | AppOp2Expr !Operator2 !Expr !Expr
  | AppExpr !Expr !Expr
  | ArrayExpr ![Expr]
  | TupleExpr ![Expr]
  | IfExpr !Expr !Expr !Expr
  | LetExpr !Patt !Expr !Expr -- pattern, bound expression, body expression
  | LambdaExpr !Patt !Type !Expr -- pattern, argument type, body expression
  
  deriving (Show, Eq)


data TypedExpr
  = UnitTypedExpr
  | IntTypedExpr !Integer
  | FloatTypedExpr !Double
  | BoolTypedExpr !Bool
  | VarTypedExpr !Type !String
  | AppOp1TypedExpr !Type !Operator1 !TypedExpr
  | AppOp2TypedExpr !Type !Operator2 !TypedExpr !TypedExpr
  | AppTypedExpr !Type !TypedExpr !TypedExpr
  | ArrayTypedExpr !Type ![TypedExpr]
  | TupleTypedExpr !Type ![TypedExpr]
  | IfTypedExpr !Type !TypedExpr !TypedExpr !TypedExpr
  | LetTypedExpr !Type !Patt !TypedExpr !TypedExpr
  | LambdaTypedExpr !Type !Patt !Type !TypedExpr
  
  deriving (Show, Eq)


typeOf :: TypedExpr -> Type
typeOf (UnitTypedExpr) = UnitType
typeOf (IntTypedExpr _) = IntType
typeOf (FloatTypedExpr _) = FloatType
typeOf (BoolTypedExpr _) = BoolType
typeOf (VarTypedExpr t _) = t
typeOf (AppOp1TypedExpr t _ _) = t
typeOf (AppOp2TypedExpr t _ _ _) = t
typeOf (AppTypedExpr t _ _) = t
typeOf (ArrayTypedExpr t _) = t
typeOf (TupleTypedExpr t _) = t
typeOf (IfTypedExpr t _ _ _) = t
typeOf (LetTypedExpr t _ _ _) = t
typeOf (LambdaTypedExpr t _ _ _) = t


data Patt
  = WildPatt
  | UnitPatt
  | VarPatt !String
  | ArrayPatt ![Patt]
  | TuplePatt ![Patt]
  
  deriving (Show, Eq)
