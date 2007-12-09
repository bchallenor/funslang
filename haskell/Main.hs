import Lexer
import Parser
import System.Environment
import Pretty

main :: IO ()
main = do a:_ <- getArgs
          s <- readFile a
          let e = (parser . lexer) s
          putStrLn (show e)
          putStrLn ""
          putStrLn (prettyExpr e)
