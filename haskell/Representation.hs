module Representation where


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
  | TOK_OP_MAP
  | TOK_OP_FOLDL
  | TOK_OP_FOLDR
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
  | AppOpExpr !Op ![Expr]
  | AppFnExpr !Expr !Expr
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
  | AppOpTypedExpr !Type !Op ![TypedExpr]
  | AppFnTypedExpr !Type !TypedExpr !TypedExpr
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
typeOf (AppOpTypedExpr t _ _) = t
typeOf (AppFnTypedExpr t _ _) = t
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


data Op
  = Op1Prefix' !Op1Prefix
  | Op1Postfix' !Op1Postfix
  | Op2Prefix' !Op2Prefix
  | Op2Infix' !Op2Infix
  | Op3Prefix' !Op3Prefix

  deriving (Show, Eq)


data Op1Prefix
  = Op1Neg
  | Op1Not
--   --
--   | Op1_sum -- i/f n -> i/f
--   | Op1_product -- i/f n -> i/f
--   | Op1_any -- bool n -> bool
--   | Op1_all -- bool n -> bool
--   | Op1_min -- i/f n -> i/f
--   | Op1_max -- i/f n -> i/f
--   --
--   | Op1_sample -- t1d -> [f 1 -> f 4]; t2d -> [f 2 -> f 4]; t3d -> [f 3 -> f 4]; tcube -> [f 3 -> f 4]
--   --
--   | Op1_len -- f n -> f
--   | Op1_dot -- f n -> [f n -> f]
--   | Op1_normalize -- f n -> f n
--   | Op1_faceforward -- f n -> [f n -> f n -> f n]
--   | Op1_reflect -- f n -> [f n -> f n]
--   | Op1_refract -- f n -> [f n -> f -> f n]

  deriving (Show, Eq)


data Op1Postfix
  = Op1Transpose
  
  deriving (Show, Eq)


data Op2Infix
  = Op2Subscript
  | Op2Swizzle
  | Op2Append
  | Op2Mul
  | Op2Div
  | Op2LinearMul
  | Op2ScaleMul
  | Op2ScaleDiv
  | Op2Add
  | Op2Sub
  | Op2LessThan
  | Op2LessThanEqual
  | Op2GreaterThan
  | Op2GreaterThanEqual
  | Op2Equal
  | Op2NotEqual
  | Op2And
  | Op2Or
  
  deriving (Show, Eq)


data Op2Prefix
  = Op2_map
  
  deriving (Show, Eq)


data Op3Prefix
  = Op3_foldl
  | Op3_foldr
  
  deriving (Show, Eq)
