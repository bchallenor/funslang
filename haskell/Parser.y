{
{-# OPTIONS -w #-} -- suppress millions of Happy warnings
module Parser(parser) where
import Representation
import Lexer
import Data.List(foldl')
}

--------------------------------------------------------------------------------
-- Directives
--------------------------------------------------------------------------------

%tokentype { Token }

%token
  BOOL { TOK_BOOL }
  INT { TOK_INT }
  FLOAT { TOK_FLOAT }
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
  OP_SUBSCRIPT { TOK_OP_SUBSCRIPT }
  OP_SWIZZLE { TOK_OP_SWIZZLE }
  OP_APPEND { TOK_OP_APPEND }
  OP_TRANSPOSE { TOK_OP_TRANSPOSE }
  OP_NOT { TOK_OP_NOT }
  OP_MUL { TOK_OP_MUL }
  OP_DIV { TOK_OP_DIV }
  OP_LINEAR_MUL { TOK_OP_LINEAR_MUL }
  OP_SCALE_MUL { TOK_OP_SCALE_MUL }
  OP_SCALE_DIV { TOK_OP_SCALE_DIV }
  OP_ADD { TOK_OP_ADD }
  OP_NEG_OP_SUB { TOK_OP_NEG_OP_SUB }
  OP_LT { TOK_OP_LT }
  OP_GT { TOK_OP_GT }
  OP_LTE { TOK_OP_LTE }
  OP_GTE { TOK_OP_GTE }
  OP_EQ { TOK_OP_EQ }
  OP_NEQ { TOK_OP_NEQ }
  OP_ID { TOK_OP_ID }
  OP_NID { TOK_OP_NID }
  OP_AND { TOK_OP_AND }
  OP_OR { TOK_OP_OR }
  IF { TOK_IF }
  THEN { TOK_THEN }
  ELSE { TOK_ELSE }
  LET { TOK_LET }
  EQUALS { TOK_EQUALS }
  IN { TOK_IN }
  TYPESPECIFIER { TOK_TYPESPECIFIER }
  RARROW { TOK_RARROW }
  LAMBDA { TOK_LAMBDA }

%name parser expr

%error { parseError }

-- Only 1 shift/reduce conflict, from the ambiguity in:
--     \ x :: Int -> 42
-- (1) \ (x :: Int) -> 42
-- (2) \ x :: (Int -> 42)
-- Shift favours (2), which fails.
-- This is fine; the user must just be explicit if they meant (1).
%expect 1

%%

--------------------------------------------------------------------------------
-- Grammar
--------------------------------------------------------------------------------
-- Note:
-- Right recursion is avoided where possible.
-- This means that in some places, lists are constructed backwards and reversed.
--------------------------------------------------------------------------------

---
--- Types
---

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
  | INT { IntType }
  | FLOAT { FloatType }
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


---
--- Expressions
---

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
  : LBRACKET LITERAL_INT RANGE_DOTS LITERAL_INT RBRACKET { ArrayExpr (map IntExpr (if $2<=$4 then [$2..$4] else reverse [$4..$2])) }
  ;

primary_expr :: { Expr }
  : LPAREN RPAREN { UnitExpr }
  | LITERAL_INT { IntExpr $1 }
  | LITERAL_FLOAT { FloatExpr $1 }
  | LITERAL_BOOL { BoolExpr $1 }
  | IDENTIFIER { VarExpr $1 }
  | tuple_expr { $1 }
  | array_expr { $1 }
  | array_range_expr { $1 }
  | LPAREN expr RPAREN { $2 }
  ;

app_expr :: { Expr }
  : app_expr primary_expr { AppExpr $1 $2 }
  | OP_NEG_OP_SUB primary_expr { AppOp1Expr Op1Neg $2 }
  | OP_NOT primary_expr { AppOp1Expr Op1Not $2 }
  | primary_expr { $1 }
  ;

postfix_expr :: { Expr }
  : postfix_expr OP_TRANSPOSE { AppOp1Expr Op1Transpose $1 }
  | app_expr { $1 }
  ;

access_expr :: { Expr }
  : access_expr OP_SUBSCRIPT postfix_expr { AppOp2Expr Op2Subscript $1 $3 }
  | access_expr OP_SWIZZLE postfix_expr { AppOp2Expr Op2Swizzle $1 $3 }
  | access_expr OP_APPEND postfix_expr { AppOp2Expr Op2Append $1 $3 }
  | postfix_expr { $1 }
  ;

multiplicative_expr :: { Expr }
  : multiplicative_expr OP_MUL access_expr { AppOp2Expr Op2Mul $1 $3 }
  | multiplicative_expr OP_DIV access_expr { AppOp2Expr Op2Div $1 $3 }
  | multiplicative_expr OP_LINEAR_MUL access_expr { AppOp2Expr Op2LinearMul $1 $3 }
  | multiplicative_expr OP_SCALE_MUL access_expr { AppOp2Expr Op2ScaleMul $1 $3 }
  | multiplicative_expr OP_SCALE_DIV access_expr { AppOp2Expr Op2ScaleDiv $1 $3 }
  | access_expr { $1 }
  ;

additive_expr :: { Expr }
  : additive_expr OP_ADD multiplicative_expr { AppOp2Expr Op2Add $1 $3 }
  | additive_expr OP_NEG_OP_SUB multiplicative_expr { AppOp2Expr Op2Sub $1 $3 }
  | multiplicative_expr { $1 }
  ;

relational_expr :: { Expr }
  : relational_expr OP_LT additive_expr { AppOp2Expr Op2LessThan $1 $3 }
  | relational_expr OP_GT additive_expr { AppOp2Expr Op2GreaterThan $1 $3 }
  | relational_expr OP_LTE additive_expr { AppOp2Expr Op2LessThanEqual $1 $3 }
  | relational_expr OP_GTE additive_expr { AppOp2Expr Op2GreaterThanEqual $1 $3 }
  | additive_expr { $1 }
  ;

equality_expr :: { Expr }
  : equality_expr OP_EQ relational_expr { AppOp2Expr Op2Equal $1 $3 }
  | equality_expr OP_NEQ relational_expr { AppOp2Expr Op2NotEqual $1 $3 }
  | relational_expr { $1 }
  ;

identity_expr :: { Expr }
  : identity_expr OP_ID equality_expr { AppOp2Expr Op2Identical $1 $3 }
  | identity_expr OP_NID equality_expr { AppOp2Expr Op2NotIdentical $1 $3 }
  | equality_expr { $1 }
  ;

logical_and_expr :: { Expr }
  : logical_and_expr OP_AND identity_expr { AppOp2Expr Op2And $1 $3 }
  | identity_expr { $1 }
  ;

logical_or_expr :: { Expr }
  : logical_or_expr OP_OR logical_and_expr { AppOp2Expr Op2Or $1 $3 }
  | logical_and_expr { $1 }
  ;

expr :: { Expr }
  : LAMBDA typed_patts RARROW expr { foldl' (\e (p, t) -> LambdaExpr p t e) $4 $2 }
  | IF expr THEN expr ELSE expr { IfExpr $2 $4 $6 }
  | LET untyped_patt EQUALS expr IN expr { LetExpr $2 $4 $6 } -- todo: function defs
  | LET IDENTIFIER typed_patts EQUALS expr IN expr { LetExpr (VarPatt $2) (foldl' (\e (p, t) -> LambdaExpr p t e) $5 $3) $7 }
  | logical_or_expr { $1 }
  ;


---
--- Patterns (with and without type annotations)
---

untyped_tuple_patt_inner :: { [Patt] }
  : untyped_patt COMMA untyped_patt { $3:$1:[] }
  | untyped_tuple_patt_inner COMMA untyped_patt { $3:$1 }
  ;

untyped_tuple_patt :: { Patt }
  : LPAREN untyped_tuple_patt_inner RPAREN { TuplePatt (reverse $2) }
  ;

untyped_array_patt_inner :: { [Patt] }
  : untyped_patt { $1:[] }
  | untyped_array_patt_inner COMMA untyped_patt { $3:$1 }
  ;

untyped_array_patt :: { Patt }
  : LBRACKET untyped_array_patt_inner RBRACKET { ArrayPatt (reverse $2) }
  ;

untyped_patt :: { Patt }
  : LPAREN RPAREN { UnitPatt }
  | IDENTIFIER { VarPatt $1 }
  | untyped_tuple_patt { $1 }
  | untyped_array_patt { $1 }
  | LPAREN untyped_patt RPAREN { $2 }
  ;

typed_tuple_patt_inner :: { [(Patt, Type)] }
  : typed_patt COMMA typed_patt { $3:$1:[] }
  | typed_tuple_patt_inner COMMA typed_patt { $3:$1 }
  ;

typed_tuple_patt :: { (Patt, Type) }
  : LPAREN typed_tuple_patt_inner RPAREN { let (ps, ts) = (unzip . reverse) $2 in (TuplePatt ps, TupleType ts) }
  ;

typed_patt :: { (Patt, Type) }
  : LPAREN RPAREN { (UnitPatt, UnitType) }
  | IDENTIFIER TYPESPECIFIER type { (VarPatt $1, $3) }
  | typed_tuple_patt { $1 }
  | untyped_array_patt TYPESPECIFIER type { ($1, $3) }
  | LPAREN typed_patt RPAREN { $2 }
  ;

typed_patts :: { [(Patt, Type)] }
  : typed_patt { $1:[] }
  | typed_patts typed_patt { $2:$1 }
  ;


--------------------------------------------------------------------------------
-- Trailer
--------------------------------------------------------------------------------

{
parseError :: [Token] -> a
parseError ts = error ("Parse error at: " ++ show ts)
}