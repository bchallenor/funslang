import Lexer
import Parser
import System.Environment

main = do a:as <- getArgs
          s <- readFile a
          putStrLn (show ((parser . lexer) s))
