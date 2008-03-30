-- Standalone compiler.
module Main(main, run) where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import System.IO
import System.Environment
import System.Console.GetOpt

import Representation
import Dataflow
import Compiler
import Pretty
import Library
import CompileError
import Test


data Flag
  = FlagGraph
  | FlagCompile
  | FlagEval
  | FlagInteractive
  | FlagLibrary
  | FlagTest
  
  deriving (Show, Eq)

opts :: [OptDescr Flag]
opts = [
  Option ['g'] [] (NoArg FlagGraph) "emit dataflow graph (must be combined with -c or -e)",
  Option ['c'] [] (NoArg FlagCompile) "compile and link vertex and fragment shaders from separate files",
  Option ['e'] [] (NoArg FlagEval) "evaluate expression from file",
  Option ['i'] [] (NoArg FlagInteractive) "enter interactive environment",
  Option ['t'] [] (NoArg FlagTest) "run automated tests",
  Option ['l'] [] (NoArg FlagLibrary) "print library in LaTeX format"
  ]

printUsage :: IO ()
printUsage = putStrLn $ usageInfo "Usage: main [options] [input files]" opts


standaloneCompile :: String -> String -> Maybe String -> Maybe String -> IO ()
standaloneCompile vsrc_path fsrc_path opt_vgraph_path opt_fgraph_path = do
  vsrc <- ByteString.readFile vsrc_path
  fsrc <- ByteString.readFile fsrc_path
  case compile vsrc fsrc of
    Right (vt, vinfo, vgraph, vemit, ft, finfo, fgraph, femit) -> do
      printResult opt_vgraph_path vt vinfo vgraph vemit
      printResult opt_fgraph_path ft finfo fgraph femit
    Left err -> putStrLn $ getErrorString err

standaloneEval :: String -> Maybe String -> IO ()
standaloneEval src_path opt_graph_path = do
  src <- ByteString.readFile src_path
  case evaluate library src of
    Right (CommandResult _ t value info) -> do
      let graph = dependencyGraph value info
      printResult opt_graph_path t info graph (show value)
    Left err -> putStrLn $ getErrorString err


printResult :: Maybe String -> Type -> InterpretState -> DFGraph -> String -> IO ()
printResult opt_graph_path t info graph result = do
  putStrLn $ prettyType t
  putStrLn $ "input/output info: " ++ show info
  case opt_graph_path of
    Just graph_path -> do
      putStrLn "outputting graphviz..."
      success <- graphvizCompile graph graph_path "png"
      putStrLn $ show success
    Nothing -> do
      putStrLn "skipping graphviz."
  putStrLn $ result


printLibrary :: IO ()
printLibrary = do
  putStrLn docLibrary


interactiveEnvironment :: Library -> IO ()
interactiveEnvironment library' = do
  putStr "\n> "
  hFlush stdout
  s <- getLine
  case evaluate library' (ByteString.pack s) of
    Right (CommandResult library'' t value _) -> do
      putStrLn $ show value
      putStrLn $ prettyType t
      interactiveEnvironment library''
    Left err -> do
      putStrLn $ getErrorString err
      interactiveEnvironment library'


dispatch :: [Flag] -> [String] -> IO ()

dispatch [FlagGraph, FlagCompile] [vsrc_path, fsrc_path] = standaloneCompile vsrc_path fsrc_path (Just "vertex-graph") (Just "fragment-graph")
dispatch [FlagCompile, FlagGraph] [vsrc_path, fsrc_path] = standaloneCompile vsrc_path fsrc_path (Just "vertex-graph") (Just "fragment-graph")
dispatch [FlagCompile] [vsrc_path, fsrc_path] = standaloneCompile vsrc_path fsrc_path Nothing Nothing

dispatch [FlagGraph, FlagEval] [src_path] = standaloneEval src_path (Just "expr-graph")
dispatch [FlagEval, FlagGraph] [src_path] = standaloneEval src_path (Just "expr-graph")
dispatch [FlagEval] [src_path] = standaloneEval src_path Nothing

dispatch [FlagLibrary] [] = printLibrary

dispatch [FlagInteractive] [] = interactiveEnvironment library

dispatch [FlagTest] [] = doTestGroups

dispatch _ _ = printUsage


main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute opts args of
    (flags, files, []) -> dispatch flags files
    (_, _, errs) -> do
      mapM putStr errs
      printUsage

run :: String -> IO ()
run s = withArgs [s] main
