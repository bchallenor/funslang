{
{-# OPTIONS -w #-} -- suppress millions of Happy warnings
module Parser(parser) where
import Representation
import Library
import Lexer
import Data.List(foldl')
}

--------------------------------------------------------------------------------
-- Directives
--------------------------------------------------------------------------------

%tokentype { Token }

%token
  REAL { TOK_REAL }
  BOOL { TOK_BOOL }
  TEXTURE1D { TOK_TEXTURE1D }
  TEXTURE2D { TOK_TEXTURE2D }
  TEXTURE3D { TOK_TEXTURE3D }
  TEXTURECUBE { TOK_TEXTURECUBE }
  LITERAL_BOOL { TOK_LITERAL_BOOL $$ }
  LITERAL_INT { TOK_LITERAL_INT $$ }
  LITERAL_FLOAT { TOK_LITERAL_FLOAT $$ }
  IDENTIFIER { TOK_IDENTIFIER $$ }
  COMMA { TOK_COMMA }
  RANGE_DOTS { TOK_RANGE_DOTS }
  LBRACKET { TOK_LBRACKET }
  RBRACKET { TOK_RBRACKET }
  LPAREN { TOK_LPAREN }
  RPAREN { TOK_RPAREN }
  WILDCARD { TOK_WILDCARD }
  OP_NOT { TOK_OP_NOT }
  OP_SUBSCRIPT { TOK_OP_SUBSCRIPT }
  OP_SWIZZLE { TOK_OP_SWIZZLE }
  OP_SCALAR_ADD { TOK_OP_SCALAR_ADD }
  OP_SCALAR_NEG_OP_SCALAR_SUB { TOK_OP_SCALAR_NEG_OP_SCALAR_SUB }
  OP_SCALAR_MUL { TOK_OP_SCALAR_MUL }
  OP_SCALAR_DIV { TOK_OP_SCALAR_DIV }
  OP_VECTOR_ADD { TOK_OP_VECTOR_ADD }
  OP_VECTOR_NEG_OP_VECTOR_SUB { TOK_OP_VECTOR_NEG_OP_VECTOR_SUB }
  OP_VECTOR_MUL { TOK_OP_VECTOR_MUL }
  OP_VECTOR_DIV { TOK_OP_VECTOR_DIV }
  OP_VECTOR_SCALAR_MUL { TOK_OP_VECTOR_SCALAR_MUL }
  OP_VECTOR_SCALAR_DIV { TOK_OP_VECTOR_SCALAR_DIV }
  OP_MATRIX_MATRIX_LINEAR_MUL { TOK_OP_MATRIX_MATRIX_LINEAR_MUL }
  OP_MATRIX_VECTOR_LINEAR_MUL { TOK_OP_MATRIX_VECTOR_LINEAR_MUL }
  OP_VECTOR_MATRIX_LINEAR_MUL { TOK_OP_VECTOR_MATRIX_LINEAR_MUL }
  OP_LT { TOK_OP_LT }
  OP_GT { TOK_OP_GT }
  OP_LTE { TOK_OP_LTE }
  OP_GTE { TOK_OP_GTE }
  OP_EQ { TOK_OP_EQ }
  OP_NEQ { TOK_OP_NEQ }
  OP_AND { TOK_OP_AND }
  OP_OR { TOK_OP_OR }
  OP_TRANSPOSE { TOK_OP_TRANSPOSE }
  IF { TOK_IF }
  THEN { TOK_THEN }
  ELSE { TOK_ELSE }
  LET { TOK_LET }
  EQUALS { TOK_EQUALS }
  IN { TOK_IN }
  TYPESPECIFIER { TOK_TYPESPECIFIER }
  RARROW { TOK_RARROW }
  LAMBDA { TOK_LAMBDA }
  LAMBDA_DOT { TOK_LAMBDA_DOT }

%left OP_OR
%left OP_AND
%nonassoc OP_EQ OP_NEQ
%nonassoc OP_LT OP_LTE OP_GT OP_GTE
%left OP_SCALAR_ADD OP_SCALAR_NEG_OP_SCALAR_SUB OP_VECTOR_ADD OP_VECTOR_NEG_OP_VECTOR_SUB
%right OP_MATRIX_VECTOR_LINEAR_MUL
%left OP_SCALAR_MUL OP_SCALAR_DIV OP_VECTOR_MUL OP_VECTOR_DIV OP_VECTOR_SCALAR_MUL OP_VECTOR_SCALAR_DIV OP_MATRIX_MATRIX_LINEAR_MUL OP_VECTOR_MATRIX_LINEAR_MUL
%left OP_SUBSCRIPT OP_SWIZZLE
%nonassoc OP_NOT
%nonassoc OP_TRANSPOSE

%name parser expr

%error { parseError }

%%

--------------------------------------------------------------------------------
-- Grammar
--------------------------------------------------------------------------------
-- Note:
-- Right recursion is avoided where possible.
-- This means that in some places, lists are constructed backwards and reversed.
--------------------------------------------------------------------------------

--
-- Types
--

tuple_type_inner :: { [Type] }
  : type COMMA type { $3:$1:[] }
  | tuple_type_inner COMMA type { $3:$1 }
  ;

tuple_type :: { Type }
  : LPAREN tuple_type_inner RPAREN { TupleType (reverse $2) }
  ;

array_type :: { Type }
  : primary_type LITERAL_INT { ArrayType $1 $2 } --todo: error on zero
  ;

primary_type :: { Type }
  : LPAREN RPAREN { UnitType }
  | REAL { RealType }
  | BOOL { BoolType }
  | TEXTURE1D { Texture1DType }
  | TEXTURE2D { Texture2DType }
  | TEXTURE3D { Texture3DType }
  | TEXTURECUBE { TextureCubeType }
  | tuple_type { $1 }
  | array_type { $1 }
  | LPAREN type RPAREN { $2 }
  ;

type :: { Type } -- right recursion for right associativity
  : primary_type RARROW type { FunType $1 $3 }
  | primary_type { $1 }
  ;


--
-- Operators
--
-- Note that the prefix negation operators cannot be sectioned.
--

operator :: { Operator }
  : OP_NOT { OpNot }
  --
  | OP_SUBSCRIPT { OpSubscript }
  | OP_SWIZZLE { OpSwizzle }
  | OP_SCALAR_ADD { OpScalarAdd }
  | OP_SCALAR_NEG_OP_SCALAR_SUB { OpScalarSub }
  | OP_SCALAR_MUL { OpScalarMul }
  | OP_SCALAR_DIV { OpScalarDiv }
  | OP_VECTOR_ADD { OpVectorAdd }
  | OP_VECTOR_NEG_OP_VECTOR_SUB { OpVectorSub }
  | OP_VECTOR_MUL { OpVectorMul }
  | OP_VECTOR_DIV { OpVectorDiv }
  | OP_VECTOR_SCALAR_MUL { OpVectorScalarMul }
  | OP_VECTOR_SCALAR_DIV { OpVectorScalarDiv }
  | OP_MATRIX_MATRIX_LINEAR_MUL { OpMatrixMatrixLinearMul }
  | OP_MATRIX_VECTOR_LINEAR_MUL { OpMatrixVectorLinearMul }
  | OP_VECTOR_MATRIX_LINEAR_MUL { OpVectorMatrixLinearMul }
  | OP_LT { OpLessThan }
  | OP_GT { OpGreaterThan }
  | OP_LTE { OpLessThanEqual }
  | OP_GTE { OpGreaterThanEqual }
  | OP_EQ { OpEqual }
  | OP_NEQ { OpNotEqual }
  | OP_AND { OpAnd }
  | OP_OR { OpOr }
  --
  | OP_TRANSPOSE { OpTranspose }
  ;


--
-- Expressions
--

tuple_expr_inner :: { [Expr] }
  : expr COMMA expr { $3:$1:[] }
  | tuple_expr_inner COMMA expr { $3:$1 }
  ;

tuple_expr :: { Expr }
  : LPAREN tuple_expr_inner RPAREN { TupleExpr (reverse $2) }
  ;

array_expr_inner :: { [Expr] }
  : expr { $1:[] }
  | array_expr_inner COMMA expr { $3:$1 }
  ;

array_expr :: { Expr }
  : LBRACKET array_expr_inner RBRACKET { ArrayExpr (reverse $2) }
  ;

array_range_expr :: { Expr }
  : LBRACKET LITERAL_INT RANGE_DOTS LITERAL_INT RBRACKET { ArrayExpr (map (RealConstExpr . fromInteger) (if $2<=$4 then [$2..$4] else reverse [$4..$2])) }
  ;

primary_expr :: { Expr }
  : LPAREN RPAREN { UnitConstExpr }
  | LITERAL_INT { RealConstExpr (fromInteger $1) }
  | LITERAL_FLOAT { RealConstExpr $1 }
  | LITERAL_BOOL { BoolConstExpr $1 }
  | IDENTIFIER { VarExpr $1 }
  | tuple_expr { $1 }
  | array_expr { $1 }
  | array_range_expr { $1 }
  | LPAREN operator RPAREN { VarExpr (show $2) }
  | LPAREN expr RPAREN { $2 }
  ;

app_expr :: { Expr }
  : app_expr primary_expr { AppExpr $1 $2 }
  | primary_expr { $1 }
  ;

operator_expr :: { Expr }
  : OP_SCALAR_NEG_OP_SCALAR_SUB operator_expr { prefixExpr OpScalarNeg $2 }
  | OP_VECTOR_NEG_OP_VECTOR_SUB operator_expr { prefixExpr OpVectorNeg $2 }
  | OP_NOT operator_expr { prefixExpr OpNot $2 }
  --
  | operator_expr OP_SUBSCRIPT operator_expr { infixExpr OpSubscript $1 $3 }
  | operator_expr OP_SWIZZLE operator_expr { infixExpr OpSwizzle $1 $3 }
  | operator_expr OP_SCALAR_ADD operator_expr { infixExpr OpScalarAdd $1 $3 }
  | operator_expr OP_SCALAR_NEG_OP_SCALAR_SUB operator_expr { infixExpr OpScalarSub $1 $3 }
  | operator_expr OP_SCALAR_MUL operator_expr { infixExpr OpScalarMul $1 $3 }
  | operator_expr OP_SCALAR_DIV operator_expr { infixExpr OpScalarDiv $1 $3 }
  | operator_expr OP_VECTOR_ADD operator_expr { infixExpr OpVectorAdd $1 $3 }
  | operator_expr OP_VECTOR_NEG_OP_VECTOR_SUB operator_expr { infixExpr OpVectorSub $1 $3 }
  | operator_expr OP_VECTOR_MUL operator_expr { infixExpr OpVectorMul $1 $3 }
  | operator_expr OP_VECTOR_DIV operator_expr { infixExpr OpVectorDiv $1 $3 }
  | operator_expr OP_VECTOR_SCALAR_MUL operator_expr { infixExpr OpVectorScalarMul $1 $3 }
  | operator_expr OP_VECTOR_SCALAR_DIV operator_expr { infixExpr OpVectorScalarDiv $1 $3 }
  | operator_expr OP_MATRIX_MATRIX_LINEAR_MUL operator_expr { infixExpr OpMatrixMatrixLinearMul $1 $3 }
  | operator_expr OP_MATRIX_VECTOR_LINEAR_MUL operator_expr { infixExpr OpMatrixVectorLinearMul $1 $3 }
  | operator_expr OP_VECTOR_MATRIX_LINEAR_MUL operator_expr { infixExpr OpVectorMatrixLinearMul $1 $3 }
  | operator_expr OP_LT operator_expr { infixExpr OpLessThan $1 $3 }
  | operator_expr OP_GT operator_expr { infixExpr OpGreaterThan $1 $3 }
  | operator_expr OP_LTE operator_expr { infixExpr OpLessThanEqual $1 $3 }
  | operator_expr OP_GTE operator_expr { infixExpr OpGreaterThanEqual $1 $3 }
  | operator_expr OP_EQ operator_expr { infixExpr OpEqual $1 $3 }
  | operator_expr OP_NEQ operator_expr { infixExpr OpNotEqual $1 $3 }
  | operator_expr OP_AND operator_expr { infixExpr OpAnd $1 $3 }
  | operator_expr OP_OR operator_expr { infixExpr OpOr $1 $3 }
  --
  | operator_expr OP_TRANSPOSE { postfixExpr OpTranspose $1 }
  | app_expr { $1 }
  ;

expr :: { Expr }
  : LAMBDA patts LAMBDA_DOT expr { foldl' (flip LambdaExpr) $4 $2 }
  | IF expr THEN expr ELSE expr { IfExpr $2 $4 $6 }
  | LET patt EQUALS expr IN expr { LetExpr $2 $4 $6 }
  | LET IDENTIFIER patts EQUALS expr IN expr { LetExpr (VarPatt $2) (foldl' (flip LambdaExpr) $5 $3) $7 }
  | operator_expr { $1 }
  ;


--
-- Patterns
--

tuple_patt_inner :: { [Patt] }
  : patt COMMA patt { $3:$1:[] }
  | tuple_patt_inner COMMA patt { $3:$1 }
  ;

tuple_patt :: { Patt }
  : LPAREN tuple_patt_inner RPAREN { TuplePatt (reverse $2) }
  ;

array_patt_inner :: { [Patt] }
  : patt { $1:[] }
  | array_patt_inner COMMA patt { $3:$1 }
  ;

array_patt :: { Patt }
  : LBRACKET array_patt_inner RBRACKET { ArrayPatt (reverse $2) }
  ;

patt :: { Patt }
  : WILDCARD { WildPatt }
  | LPAREN RPAREN { UnitPatt }
  | IDENTIFIER { VarPatt $1 }
  | tuple_patt { $1 }
  | array_patt { $1 }
  | LPAREN patt RPAREN { $2 }
  ;

patts :: { [Patt] }
  : patt { $1:[] }
  | patts patt { $2:$1 }
  ;


--------------------------------------------------------------------------------
-- Trailer
--------------------------------------------------------------------------------

{
parseError :: [Token] -> a
parseError ts = error ("Parse error at: " ++ show ts)

prefixExpr :: Operator -> Expr -> Expr
prefixExpr op a = AppExpr (VarExpr (show op)) a

infixExpr :: Operator -> Expr -> Expr -> Expr
infixExpr op a b = AppExpr (AppExpr (VarExpr (show op)) a) b

postfixExpr :: Operator -> Expr -> Expr
postfixExpr op a = AppExpr (VarExpr (show op)) a
}