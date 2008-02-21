-- Standalone compiler.
module Main(main) where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import System.IO
import System.Environment
import System.Console.GetOpt

import Representation
import Dataflow
import Compiler
import Pretty


data Flag
  = FlagGraph
  | FlagCompile
  | FlagEval
  
  deriving (Show, Eq)

opts :: [OptDescr Flag]
opts = [
  Option ['g'] [] (NoArg FlagGraph) "emit dataflow graph",
  Option ['c'] [] (NoArg FlagCompile) "compile and link vertex and fragment shaders",
  Option ['e'] [] (NoArg FlagEval) "evaluate expression"
  ]

printUsage :: IO ()
printUsage = putStrLn $ usageInfo "Usage: main [options] files" opts


standaloneCompile :: String -> String -> Maybe String -> Maybe String -> IO ()
standaloneCompile vsrc_path fsrc_path opt_vgraph_path opt_fgraph_path = do
  vsrc <- ByteString.readFile vsrc_path
  fsrc <- ByteString.readFile fsrc_path
  case compile vsrc fsrc of
    Right (vt, vinfo, vgraph, vemit, ft, finfo, fgraph, femit) -> do
      printResult opt_vgraph_path vt vinfo vgraph vemit
      printResult opt_fgraph_path ft finfo fgraph femit
    Left msg -> putStrLn msg

standaloneEval :: String -> Maybe String -> IO ()
standaloneEval src_path opt_graph_path = do
  src <- ByteString.readFile src_path
  case evaluate src of
    Right (t, info, graph, value) -> do
      printResult opt_graph_path t info graph (show value)
    Left msg -> putStrLn msg


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


dispatch :: [Flag] -> [String] -> IO ()

dispatch [FlagGraph, FlagCompile] [vsrc_path, fsrc_path] = standaloneCompile vsrc_path fsrc_path (Just "vertex-graph") (Just "fragment-graph")
dispatch [FlagCompile, FlagGraph] [vsrc_path, fsrc_path] = standaloneCompile vsrc_path fsrc_path (Just "vertex-graph") (Just "fragment-graph")
dispatch [FlagCompile] [vsrc_path, fsrc_path] = standaloneCompile vsrc_path fsrc_path Nothing Nothing

dispatch [FlagGraph, FlagEval] [src_path] = standaloneEval src_path (Just "expr-graph")
dispatch [FlagEval, FlagGraph] [src_path] = standaloneEval src_path (Just "expr-graph")
dispatch [FlagEval] [src_path] = standaloneEval src_path Nothing

dispatch _ _ = printUsage


main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute opts args of
    (flags, files, []) -> dispatch flags files
    (_, _, errs) -> do
      mapM putStr errs
      printUsage
