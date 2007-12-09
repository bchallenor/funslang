import Lexer
import Parser
import System.Environment
import Pretty
import Representation

main = do a:as <- getArgs
          s <- readFile a
          let Program e = (parser . lexer) s
          putStrLn (show e)
          putStrLn ""
          putStrLn (prettyExpr e)
