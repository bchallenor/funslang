{
{-# OPTIONS -w #-} -- suppress millions of Alex warnings
module Lexer where
-- You shouldn't be importing this module; everything you need is in Parser

import qualified Data.ByteString.Lazy.Char8 as ByteString

import Representation
}


-- Character sets.
$d = [0-9]
$a = [a-zA-Z]
$s = [\ \t\n\f\v\r]

-- Regex macros.
@ident = [$a _] [$a $d _ ']*


-- Token rules.
tokens :-
  
  $s+                       ; -- ignore whitespace
  "%" .*                    ; -- ignore comments (upto newline)
  
  "Bool"                    { \ (_, _, bs) len -> return $ TOK_BOOL }
  "Real"                    { \ (_, _, bs) len -> return $ TOK_REAL }
  
  "Texture1D"               { \ (_, _, bs) len -> return $ TOK_TEXTURE1D }
  "Texture2D"               { \ (_, _, bs) len -> return $ TOK_TEXTURE2D }
  "Texture3D"               { \ (_, _, bs) len -> return $ TOK_TEXTURE3D }
  "TextureCube"             { \ (_, _, bs) len -> return $ TOK_TEXTURECUBE }
  
  "True"                    { \ (_, _, bs) len -> return $ TOK_LITERAL_BOOL True }
  "False"                   { \ (_, _, bs) len -> return $ TOK_LITERAL_BOOL False }
  "-"? $d+                  { \ (_, _, bs) len -> return $ TOK_LITERAL_INT (let Just (i, _) = ByteString.readInteger $ ByteString.take (fromIntegral len) bs in i) }
  "-"? $d+ "." $d+          { \ (_, _, bs) len -> return $ TOK_LITERAL_FLOAT (read (ByteString.unpack $ ByteString.take (fromIntegral len) bs) :: Double) }
  
  ","                       { \ (_, _, bs) len -> return $ TOK_COMMA }
  ".."                      { \ (_, _, bs) len -> return $ TOK_RANGE_DOTS }
  "["                       { \ (_, _, bs) len -> return $ TOK_LBRACKET }
  "]"                       { \ (_, _, bs) len -> return $ TOK_RBRACKET }
  "("                       { \ (_, _, bs) len -> return $ TOK_LPAREN }
  ")"                       { \ (_, _, bs) len -> return $ TOK_RPAREN }
  "`"                       { \ (_, _, bs) len -> return $ TOK_BACKTICK }
  "_"                       { \ (_, _, bs) len -> return $ TOK_WILDCARD }
  
  "!"                       { \ (_, _, bs) len -> return $ TOK_OP_SUBSCRIPT }
  "!!"                      { \ (_, _, bs) len -> return $ TOK_OP_SWIZZLE }
  "+"                       { \ (_, _, bs) len -> return $ TOK_OP_SCALAR_ADD }
  "-"                       { \ (_, _, bs) len -> return $ TOK_OP_SCALAR_NEG_OP_SCALAR_SUB }
  "*"                       { \ (_, _, bs) len -> return $ TOK_OP_SCALAR_MUL }
  "/"                       { \ (_, _, bs) len -> return $ TOK_OP_SCALAR_DIV }
  "++"                      { \ (_, _, bs) len -> return $ TOK_OP_VECTOR_ADD }
  "--"                      { \ (_, _, bs) len -> return $ TOK_OP_VECTOR_NEG_OP_VECTOR_SUB }
  "**"                      { \ (_, _, bs) len -> return $ TOK_OP_VECTOR_MUL }
  "//"                      { \ (_, _, bs) len -> return $ TOK_OP_VECTOR_DIV }
  "**."                     { \ (_, _, bs) len -> return $ TOK_OP_VECTOR_SCALAR_MUL }
  "//."                     { \ (_, _, bs) len -> return $ TOK_OP_VECTOR_SCALAR_DIV }
  "#"                       { \ (_, _, bs) len -> return $ TOK_OP_MATRIX_MATRIX_LINEAR_MUL }
  "#."                      { \ (_, _, bs) len -> return $ TOK_OP_MATRIX_VECTOR_LINEAR_MUL }
  ".#"                      { \ (_, _, bs) len -> return $ TOK_OP_VECTOR_MATRIX_LINEAR_MUL }
  "<"                       { \ (_, _, bs) len -> return $ TOK_OP_LT }
  ">"                       { \ (_, _, bs) len -> return $ TOK_OP_GT }
  "<="                      { \ (_, _, bs) len -> return $ TOK_OP_LTE }
  ">="                      { \ (_, _, bs) len -> return $ TOK_OP_GTE }
  "=="                      { \ (_, _, bs) len -> return $ TOK_OP_EQ }
  "/="                      { \ (_, _, bs) len -> return $ TOK_OP_NEQ }
  "&&"                      { \ (_, _, bs) len -> return $ TOK_OP_AND }
  "||"                      { \ (_, _, bs) len -> return $ TOK_OP_OR }
  "$"                       { \ (_, _, bs) len -> return $ TOK_OP_APPLY }
  
  "if"                      { \ (_, _, bs) len -> return $ TOK_IF }
  "then"                    { \ (_, _, bs) len -> return $ TOK_THEN }
  "else"                    { \ (_, _, bs) len -> return $ TOK_ELSE }
  "let"                     { \ (_, _, bs) len -> return $ TOK_LET }
  "="                       { \ (_, _, bs) len -> return $ TOK_EQUALS }
  "in"                      { \ (_, _, bs) len -> return $ TOK_IN }
  
  "::"                      { \ (_, _, bs) len -> return $ TOK_TYPESPECIFIER }
  "->"                      { \ (_, _, bs) len -> return $ TOK_RARROW }
  "\"                       { \ (_, _, bs) len -> return $ TOK_LAMBDA }
  "."                       { \ (_, _, bs) len -> return $ TOK_LAMBDA_DOT }
  
  -- this goes last as it should not take precedence over the keywords
  @ident                    { \ (_, _, bs) len -> return $ TOK_IDENTIFIER $ ByteString.unpack $ ByteString.take (fromIntegral len) bs }


{
data AlexPos
  = AlexPos
    !Int -- address (number of characters preceding the token)
    !Int -- line number
    !Int -- column number (assuming alexTabSize)
  deriving (Eq,Show)

alexTabSize :: Int
alexTabSize = 2

type AlexInput
  = (
    AlexPos, -- current position
    Char, -- previous char
    ByteString.ByteString -- current input string
  )

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,_,cs) | ByteString.null cs = Nothing
                     | otherwise = let c   = ByteString.head cs
                                       cs' = ByteString.tail cs
                                       p'  = alexMove p c
                                    in p' `seq` cs' `seq` Just (c, (p', c, cs'))

alexStartPos :: AlexPos
alexStartPos = AlexPos 0 1 1

alexStartCode :: Int
alexStartCode = 0

alexStartChr :: Char
alexStartChr = '\n'

alexMove :: AlexPos -> Char -> AlexPos
alexMove (AlexPos a l c) '\t' = AlexPos (a+1)  l     (((c+alexTabSize-1) `div` alexTabSize)*alexTabSize+1)
alexMove (AlexPos a l c) '\n' = AlexPos (a+1) (l+1)   1
alexMove (AlexPos a l c) _    = AlexPos (a+1)  l     (c+1)


-- The parser state.
data PState
  = PState
    {
      alex_inp :: AlexInput, -- the current input
      fresh_vrefs :: ([TypeVarRef], [DimVarRef]) -- fresh variable references to use when constructing types
    }
  deriving Show

-- The parser return type.
data PResult a
  = POk !PState !a
  | PFailed !PState !String
  deriving Show

-- The parser monad.
newtype P a = P { unP :: PState -> PResult a }

instance Monad P where
  return = returnP
  (>>=) = thenP
  fail = failP

returnP :: a -> P a
returnP a = P $ \s -> POk s a

thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \ s ->
  case m s of
    POk s1 a -> (unP (k a)) s1
    PFailed s msg -> PFailed s msg

failP :: String -> P a
failP msg = P $ \s -> PFailed s msg

getInputP :: P AlexInput
getInputP = P $ \s @ PState{ alex_inp = inp } -> POk s inp

setInputP :: AlexInput -> P ()
setInputP inp = P $ \s -> POk s{ alex_inp = inp } ()

getLineColP :: P (Int, Int)
getLineColP = P $ \s @ PState{ alex_inp = (AlexPos _ l c,_,_) } -> POk s (l, c)

getFreshVarRefsP :: P ([TypeVarRef], [DimVarRef])
getFreshVarRefsP = P $ \s @ PState{ fresh_vrefs = vrefs } -> POk s vrefs

putFreshVarRefsP :: ([TypeVarRef], [DimVarRef]) -> P ()
putFreshVarRefsP vrefs = P $ \s -> POk s{ fresh_vrefs = vrefs } ()


-- Monadic lexer wrapper (as Happy wants it in continuation passing style).
lexer :: (Token -> P a) -> P a
lexer cont = do
  tok <- lexToken
  cont tok

-- Monadic lexer.
lexToken :: P Token
lexToken = do
  inp <- getInputP
  case alexScan inp alexStartCode of
    AlexEOF -> return TOK_EOF
    AlexError (_,_,bs) -> lexError $ ByteString.index bs 0
    AlexSkip  inp' len -> do
      setInputP inp'
      lexToken
    AlexToken inp' len action -> do
      setInputP inp'
      action inp len

-- Lexer error function.
lexError :: Char -> P a
lexError c = fail $ "lex error at <" ++ [c] ++ ">"
}