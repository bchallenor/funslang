alex -g Lexer.x
happy -g -c Parser.y
ghc --make Main
pause