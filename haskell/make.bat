alex -g Lexer.x
happy -g -c Parser.y
ghc -Wall -Werror --make Main