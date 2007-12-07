{
module Parser(parser) where
import Representation
import Lexer
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
  VERTICAL_BAR { TOK_VERTICAL_BAR }
  LBRACKET { TOK_LBRACKET }
  RBRACKET { TOK_RBRACKET }
  LPAREN { TOK_LPAREN }
  RPAREN { TOK_RPAREN }
  OP_SUBSCRIPT { TOK_OP_SUBSCRIPT }
  OP_SWIZZLE { TOK_OP_SWIZZLE }
  OP_APPEND { TOK_OP_APPEND }
  OP_TRANSPOSE { TOK_OP_TRANSPOSE }
  OP_MUL { TOK_OP_MUL }
  OP_DIV { TOK_OP_DIV }
  OP_LINEAR_MUL { TOK_OP_LINEAR_MUL }
  OP_SCALE_MUL { TOK_OP_SCALE_MUL }
  OP_SCALE_DIV { TOK_OP_SCALE_DIV }
  OP_ADD { TOK_OP_ADD }
  OP_SUBNEG { TOK_OP_SUBNEG }
  OP_LT { TOK_OP_LT }
  OP_GT { TOK_OP_GT }
  OP_LTE { TOK_OP_LTE }
  OP_GTE { TOK_OP_GTE }
  OP_EQ { TOK_OP_EQ }
  OP_NEQ { TOK_OP_NEQ }
  OP_ID { TOK_OP_ID }
  OP_NID { TOK_OP_NID }
  OP_AND { TOK_OP_AND }
  OP_XOR { TOK_OP_XOR }
  OP_OR { TOK_OP_OR }
  IF { TOK_IF }
  THEN { TOK_THEN }
  ELSE { TOK_ELSE }
  LET { TOK_LET }
  EQUALS { TOK_EQUALS }
  IN { TOK_IN }
  UPTO { TOK_UPTO }
  TYPESPECIFIER { TOK_TYPESPECIFIER }
  UNIFORM { TOK_UNIFORM }
  TEXTURE { TOK_TEXTURE }
  FUN { TOK_FUN }
  KERNEL { TOK_KERNEL }
  VSHADER { TOK_VSHADER }
  FSHADER { TOK_FSHADER }

%name parser program

%expect 1 -- the if/then/else and let/=/in state; accept default resolution

%error { parseError }

%%

--------------------------------------------------------------------------------
-- Grammar
--------------------------------------------------------------------------------
-- Note:
-- Right recursion is avoided where possible.
-- This means that in some places, lists need reversing.
--------------------------------------------------------------------------------

---
--- Types
---

texture_type :: { Type }
  : TEXTURE1D { Texture1DType }
  | TEXTURE2D { Texture2DType }
  | TEXTURE3D { Texture3DType }
  | TEXTURECUBE { TextureCubeType }
  | texture_type LITERAL_INT { ArrayType $1 $2 }
  ;

boolean_type :: { Type }
  : BOOL { BoolType }
  | boolean_type LITERAL_INT { ArrayType $1 $2 }
  ;

integral_type :: { Type }
  : INT { IntType }
  | integral_type LITERAL_INT { ArrayType $1 $2 }
  ;

floating_type :: { Type }
  : FLOAT { FloatType }
  | floating_type LITERAL_INT { ArrayType $1 $2 }
  ;

arithboolean_type :: { Type }
  : boolean_type { $1 }
  | integral_type { $1 }
  | floating_type { $1 }
  ;

basic_type :: { Type }
  : boolean_type { $1 }
  | integral_type { $1 }
  | floating_type { $1 }
  | texture_type { $1 }
  ;


---
--- Expressions
---

--- single expressions (i.e. not n-tuples)

arr_inner_expr :: { [Expr] }
  : single_expr { [$1] }
  | arr_inner_expr COMMA single_expr { $3:$1 }
  ;

arr_constructor_expr :: { Expr }
  : LBRACKET arr_inner_expr RBRACKET { ArrayConsExpr (reverse $2) }
  ;

arr_comprehension_expr :: { Expr }
  : LBRACKET single_expr VERTICAL_BAR generator RBRACKET { let (v,a,b) = $4 in ArrayCompExpr $2 v a b }
  ;

primary_expr :: { Expr }
  : LITERAL_INT { IntExpr $1 }
  | LITERAL_BOOL { BoolExpr $1 }
  | LITERAL_FLOAT { FloatExpr $1 }
  | LPAREN RPAREN { UnitExpr }
  | arr_constructor_expr { $1 }
  | arr_comprehension_expr { $1 }
  | IDENTIFIER { VarExpr $1 }
  | LPAREN expr RPAREN { $2 }
  ;

postfix_expr :: { Expr }
  : postfix_expr OP_SUBSCRIPT primary_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | postfix_expr OP_SWIZZLE primary_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | postfix_expr OP_APPEND primary_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | postfix_expr OP_TRANSPOSE { AppExpr "lib" $1 }
  | primary_expr { $1 }
  ;

prefix_expr :: { Expr }
  : OP_SUBNEG postfix_expr { AppExpr "lib" $2 }
  | IDENTIFIER postfix_expr { AppExpr $1 $2 }
  | postfix_expr { $1 }
  ;

multiplicative_expr :: { Expr }
  : multiplicative_expr OP_MUL prefix_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | multiplicative_expr OP_DIV prefix_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | multiplicative_expr OP_LINEAR_MUL prefix_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | multiplicative_expr OP_SCALE_MUL prefix_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | multiplicative_expr OP_SCALE_DIV prefix_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | prefix_expr { $1 }
  ;

additive_expr :: { Expr }
  : additive_expr OP_ADD multiplicative_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | additive_expr OP_SUBNEG multiplicative_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | multiplicative_expr { $1 }
  ;

relational_expr :: { Expr }
  : relational_expr OP_LT additive_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | relational_expr OP_GT additive_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | relational_expr OP_LTE additive_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | relational_expr OP_GTE additive_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | additive_expr { $1 }
  ;

equality_expr :: { Expr }
  : equality_expr OP_EQ relational_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | equality_expr OP_NEQ relational_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | relational_expr { $1 }
  ;

identity_expr :: { Expr }
  : identity_expr OP_ID equality_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | identity_expr OP_NID equality_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | equality_expr { $1 }
  ;

logical_and_expr :: { Expr }
  : logical_and_expr OP_AND identity_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | identity_expr { $1 }
  ;

logical_xor_expr :: { Expr }
  : logical_xor_expr OP_XOR logical_and_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | logical_and_expr { $1 }
  ;

logical_or_expr :: { Expr }
  : logical_or_expr OP_OR logical_xor_expr { AppExpr "lib" (TupleExpr [$1,$3]) }
  | logical_xor_expr { $1 }
  ;

single_expr :: { Expr }
  : IF expr THEN expr ELSE expr { IfExpr $2 $4 $6 }
  | LET pattern EQUALS expr IN expr { LetExpr $2 $4 $6 }
  | logical_or_expr { $1 }
  ;

--- n-tuple expressions

expr :: { Expr } -- warning: right recursion!
  : single_expr { $1 }
  | single_expr COMMA expr { case $3 of { TupleExpr es -> TupleExpr ($1:es); _ -> TupleExpr [$1,$3] } }
  ;


---
--- Generators
---

generator :: { (String, Expr, Expr ) }
  : IDENTIFIER EQUALS expr UPTO expr { ($1, $3, $5) }
  ;


---
--- Patterns (for let-bindings)
---

arr_inner_pattern :: { [Patt] }
  : single_pattern { [$1] }
  | arr_inner_pattern COMMA single_pattern { $3:$1 }
  ;

arr_constructor_pattern :: { Patt }
  : LBRACKET arr_inner_pattern RBRACKET { ArrayConsPatt (reverse $2) }
  ;

primary_pattern :: { Patt }
  : arr_constructor_pattern { $1 }
  | IDENTIFIER { VarPatt $1 }
  | LPAREN pattern RPAREN { $2 }
  ;

single_pattern :: { Patt }
  : primary_pattern { $1 }
  ;

pattern :: { Patt } -- warning: right recursion!
  : single_pattern { $1 }
  | single_pattern COMMA pattern { case $3 of { TuplePatt es -> TuplePatt ($1:es); _ -> TuplePatt [$1,$3] } }
  ;


---
--- Auxiliary declarations
---

uniform_decl :: { AuxDecl }
  : UNIFORM IDENTIFIER TYPESPECIFIER arithboolean_type { UniformDecl ($2,$4) }
  ;

texture_decl :: { AuxDecl }
  : TEXTURE IDENTIFIER TYPESPECIFIER texture_type { TextureDecl ($2,$4) }
  ;

let_decl :: { AuxDecl }
  : LET pattern EQUALS expr { LetDecl $2 $4 }
  ;

fun_param :: { TypedIdent }
  : IDENTIFIER TYPESPECIFIER basic_type { ($1,$3) }
  ;

fun_params :: { [TypedIdent] }
  : fun_param { [$1] }
  | fun_params COMMA fun_param { $3:$1 }
  ;

fun_decl :: { AuxDecl }
  : FUN IDENTIFIER LPAREN fun_params RPAREN EQUALS expr { FunDecl $2 (reverse $4) $7 }
  ;

aux_decl :: { AuxDecl }
  : uniform_decl { $1 }
  | texture_decl { $1 }
  | let_decl { $1 }
  | fun_decl { $1 }
  ;

aux_decls :: { [AuxDecl] }
  : aux_decl { [$1] }
  | aux_decls aux_decl { $2:$1 }
  ;

aux_decls_opt :: { [AuxDecl] }
  : {- empty -} { [] }
  | aux_decls { reverse $1 }
  ;


---
--- Kernel declarations
---

vkernel_param :: { TypedIdent }
  : IDENTIFIER TYPESPECIFIER arithboolean_type { ($1,$3) }
  ;

vkernel_params :: { [TypedIdent] }
  : vkernel_param { [$1] }
  | vkernel_params COMMA vkernel_param { $3:$1 }
  ;

vkernel :: { (ProgramKind, KernelDecl) }
  : KERNEL VSHADER LPAREN RPAREN EQUALS expr { (VertProgram, KernelDecl [] $6) }
  | KERNEL VSHADER LPAREN vkernel_params RPAREN EQUALS expr { (VertProgram, KernelDecl (reverse $4) $7) }
  ;

fkernel_param :: { TypedIdent }
  : IDENTIFIER TYPESPECIFIER floating_type { ($1,$3) }
  ;

fkernel_params :: { [TypedIdent] }
  : fkernel_param { [$1] }
  | fkernel_params COMMA fkernel_param { $3:$1 }
  ;

fkernel :: { (ProgramKind, KernelDecl) }
  : KERNEL FSHADER LPAREN RPAREN EQUALS expr { (FragProgram, KernelDecl [] $6) }
  | KERNEL FSHADER LPAREN fkernel_params RPAREN EQUALS expr { (FragProgram, KernelDecl (reverse $4) $7) }
  ;


---
--- Program
---

program :: { Program }
  : aux_decls_opt vkernel { let (kind, kernel) = $2 in Program kind $1 kernel }
  | aux_decls_opt fkernel { let (kind, kernel) = $2 in Program kind $1 kernel }
  ;


--------------------------------------------------------------------------------
-- Trailer
--------------------------------------------------------------------------------

{
parseError :: [Token] -> a
parseError _ = error "Happy error"
}