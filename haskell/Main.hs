import Lexer

getStr = do
          x <- getChar
          if x==' ' then return "" else do {y<-getStr ; return (x : y) }

main = do c <- getStr; putStr (show (lexer c)); main