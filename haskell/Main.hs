import Lexer
import Parser

getStr = do
          x <- getChar
          if x=='\n' then return "" else do {y<-getStr ; return (x : y) }

--main = do s <- readFile "test.vp"
main = do s <- getStr
          putStr (show ((parser . lexer) s))
