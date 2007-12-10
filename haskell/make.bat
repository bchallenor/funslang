alex -g Lexer.x
happy -g -c -i Parser.y
ghc -Wall -Werror --make Main