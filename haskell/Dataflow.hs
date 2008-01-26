module Dataflow(DFGraph, dependencyGraph, graphvizCompile) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Array
import Data.Graph
import System.IO
import System.Process
import System.Exit
import Control.Exception(assert)

import Representation


-- Dependency graph.
-- Each edge (a,b) in the Graph means that b depends on a, or equivalently
-- that a must be calculated before b.
type DFGraph = (
  Graph, -- graph representation
  [DF], -- result nodes
  Map.Map DF Vertex, Map.Map Vertex DF -- translations between nodes/vertices
  )


-- Compile a GraphViz "dot" representation of a graph.
graphvizCompile :: DFGraph -> String -> String -> IO Bool
graphvizCompile g name ext = do
  let src = graphviz g
  let dotFileName = name ++ ".dot"
  let outFileName = name ++ "." ++ ext
  writeFile dotFileName src
  h <- runCommand $ "dot -T" ++ ext ++ " " ++ dotFileName ++ " -o " ++ outFileName
  exit <- waitForProcess h
  case exit of
    ExitSuccess -> return True
    ExitFailure _ -> return False


-- Return a GraphViz "dot" representation of a graph.
graphviz :: DFGraph -> String
graphviz (adjs, _, _, mvn) =
  let assoclist = assocs adjs in
    "digraph DF {"
    ++
    (concat $ map (\(v,_) -> case Map.lookup v mvn of { Just n -> "\nn" ++ show v ++ " [label=\"" ++ (nodeLabel n) ++ "\", color=\"#" ++ case n of { DFReal _ -> "800000"; DFBool _ -> "000080"; DFSample _ -> "008000" } ++ "\"];"; Nothing -> error "" }) assoclist)
    ++
    (concat $ map (\(v,vs) -> concat $ map (\v' -> "\nn" ++ show v ++ " -> n" ++ show v' ++ ";") vs) $ assoclist)
    ++
    "\n}"


-- Given a Value, makes its DFGraph.
-- This has the effect of (1) transposing the DF graph (where the edges were
-- in the other direction) and (2) removing any common subgraphs.
dependencyGraph :: Value -> DFGraph
dependencyGraph value =
  let result_ns = getResultDFs value in
  let (v', es', mnv', mvn') = dependencyEdges (0, [], Map.empty, Map.empty) result_ns in
    (buildG (0, v'-1) es', result_ns, mnv', mvn')

-- (next Vertex available for use, edges so far, map so far, map so far)
type DependencyEdgesAcc = (Vertex, [Edge], Map.Map DF Vertex, Map.Map Vertex DF)

-- Process a list of DFs in the same context.
dependencyEdges :: DependencyEdgesAcc -> [DF] -> DependencyEdgesAcc
dependencyEdges acc [] = acc
dependencyEdges acc@(_, _, mnv1, _) (n:ns) =
  -- Check whether this node has been done before.
  case Map.lookup n mnv1 of
    Just _ -> dependencyEdges acc ns
    Nothing ->
      -- Find the list of dependencies.
      let ndeps = nodeDependencies n in
      -- Process them first.
      let (v2, es2, mnv2, mvn2) = dependencyEdges acc ndeps in
      -- Now we can claim v2 as the label for this node n.
      -- Look up vdep for each dependency ndep and add an edge from the vdep to v2.
      let {(v4, es4, mnv4, mvn4) = List.foldl' (
          \(v3, es3, mnv3, mvn3) ndep ->
            case Map.lookup ndep mnv3 of
              Just vdep -> (v3, (vdep, v2):es3, mnv3, mvn3)
              Nothing -> error $ "this node should have been added already: " ++ show ndep
          ) (v2, es2, mnv2, mvn2) ndeps } in
      assert (v2 == v4)
      assert (mvn2 == mvn4)
      assert (mnv2 == mnv4)
      -- Finally, add this node to the accumulator and then recurse.
      dependencyEdges (v2+1, es4, Map.insert n v2 mnv2, Map.insert v2 n mvn2) ns


-- Gets the result DFs which represent this Value.
getResultDFs :: Value -> [DF]
getResultDFs (ValueUnit) = []
getResultDFs (ValueDFReal df) = [DFReal df]
getResultDFs (ValueDFBool df) = [DFBool df]
getResultDFs (ValueTexture1D _) = error getResultDFsErrorMsg
getResultDFs (ValueTexture2D _) = error getResultDFsErrorMsg
getResultDFs (ValueTexture3D _) = error getResultDFsErrorMsg
getResultDFs (ValueTextureCube _) = error getResultDFsErrorMsg
getResultDFs (ValueArray vs) = concat $ map getResultDFs vs
getResultDFs (ValueTuple vs) = concat $ map getResultDFs vs
getResultDFs (ValueFun _) = error getResultDFsErrorMsg
getResultDFsErrorMsg :: String
getResultDFsErrorMsg = "this value cannot be represented by a dataflow graph"


-- Given a node, returns the list of nodes that it depends on.
nodeDependencies :: DF -> [DF]

nodeDependencies (DFReal (DFRealLiteral _)) = []
nodeDependencies (DFReal (DFRealVarying _)) = []
nodeDependencies (DFReal (DFRealUniform _)) = []

nodeDependencies (DFReal (DFRealCond dfbc dfr1 dfr2)) = [DFBool dfbc, DFReal dfr1, DFReal dfr2]

nodeDependencies (DFReal (DFRealAdd dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealSub dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealMul dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealDiv dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealNeg dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealRcp dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealRsq dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealAbs dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealMin dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealMax dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealFloor dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealCeiling dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealRound dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealTruncate dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealFract dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealExp dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealExp2 dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealLog dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealLog2 dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealPow dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealSin dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealCos dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealTan dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealASin dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealACos dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealATan dfr1)) = [DFReal dfr1]

nodeDependencies (DFReal (DFRealGetTexR dfs1)) = [DFSample dfs1]
nodeDependencies (DFReal (DFRealGetTexG dfs1)) = [DFSample dfs1]
nodeDependencies (DFReal (DFRealGetTexB dfs1)) = [DFSample dfs1]
nodeDependencies (DFReal (DFRealGetTexA dfs1)) = [DFSample dfs1]

nodeDependencies (DFBool (DFBoolLiteral _)) = []
nodeDependencies (DFBool (DFBoolVarying _)) = []
nodeDependencies (DFBool (DFBoolUniform _)) = []

nodeDependencies (DFBool (DFBoolCond dfbc dfb1 dfb2)) = [DFBool dfbc, DFBool dfb1, DFBool dfb2]

nodeDependencies (DFBool (DFBoolLessThan dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFBool (DFBoolLessThanEqual dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFBool (DFBoolGreaterThan dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFBool (DFBoolGreaterThanEqual dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]

nodeDependencies (DFBool (DFBoolEqualReal dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFBool (DFBoolNotEqualReal dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFBool (DFBoolEqualBool dfb1 dfb2)) = [DFBool dfb1, DFBool dfb2]
nodeDependencies (DFBool (DFBoolNotEqualBool dfb1 dfb2)) = [DFBool dfb1, DFBool dfb2]

nodeDependencies (DFBool (DFBoolAnd dfb1 dfb2)) = [DFBool dfb1, DFBool dfb2]
nodeDependencies (DFBool (DFBoolOr dfb1 dfb2)) = [DFBool dfb1, DFBool dfb2]
nodeDependencies (DFBool (DFBoolNot dfb1)) = [DFBool dfb1]

nodeDependencies (DFSample (DFSample1D _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFSample (DFSample2D _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFSample (DFSample3D _ dfr1 dfr2 dfr3)) = [DFReal dfr1, DFReal dfr2, DFReal dfr3]
nodeDependencies (DFSample (DFSampleCube _ dfr1 dfr2 dfr3)) = [DFReal dfr1, DFReal dfr2, DFReal dfr3]


-- Given a node, returns a pretty label for visualization.
nodeLabel :: DF -> String

nodeLabel (DFReal (DFRealLiteral d)) = show d
nodeLabel (DFReal (DFRealVarying i)) = "RealVarying[" ++ show i ++ "]"
nodeLabel (DFReal (DFRealUniform i)) = "RealUniform[" ++ show i ++ "]"

nodeLabel (DFReal (DFRealCond _ _ _)) = "Cond"

nodeLabel (DFReal (DFRealAdd _ _)) = "Add"
nodeLabel (DFReal (DFRealSub _ _)) = "Sub"
nodeLabel (DFReal (DFRealMul _ _)) = "Mul"
nodeLabel (DFReal (DFRealDiv _ _)) = "Div"
nodeLabel (DFReal (DFRealNeg _)) = "Neg"
nodeLabel (DFReal (DFRealRcp _)) = "Rcp"
nodeLabel (DFReal (DFRealRsq _)) = "Rsq"
nodeLabel (DFReal (DFRealAbs _)) = "Abs"
nodeLabel (DFReal (DFRealMin _ _)) = "Min"
nodeLabel (DFReal (DFRealMax _ _)) = "Max"
nodeLabel (DFReal (DFRealFloor _)) = "Floor"
nodeLabel (DFReal (DFRealCeiling _)) = "Ceiling"
nodeLabel (DFReal (DFRealRound _)) = "Round"
nodeLabel (DFReal (DFRealTruncate _)) = "Truncate"
nodeLabel (DFReal (DFRealFract _)) = "Fract"
nodeLabel (DFReal (DFRealExp _)) = "Exp"
nodeLabel (DFReal (DFRealExp2 _)) = "Exp2"
nodeLabel (DFReal (DFRealLog _)) = "Log"
nodeLabel (DFReal (DFRealLog2 _)) = "Log2"
nodeLabel (DFReal (DFRealPow _ _)) = "Pow"
nodeLabel (DFReal (DFRealSin _)) = "Sin"
nodeLabel (DFReal (DFRealCos _)) = "Cos"
nodeLabel (DFReal (DFRealTan _)) = "Tan"
nodeLabel (DFReal (DFRealASin _)) = "ASin"
nodeLabel (DFReal (DFRealACos _)) = "ACos"
nodeLabel (DFReal (DFRealATan _)) = "ATan"

nodeLabel (DFReal (DFRealGetTexR _)) = "GetTexR"
nodeLabel (DFReal (DFRealGetTexG _)) = "GetTexG"
nodeLabel (DFReal (DFRealGetTexB _)) = "GetTexB"
nodeLabel (DFReal (DFRealGetTexA _)) = "GetTexA"

nodeLabel (DFBool (DFBoolLiteral b)) = show b
nodeLabel (DFBool (DFBoolVarying i)) = "BoolVarying[" ++ show i ++ "]"
nodeLabel (DFBool (DFBoolUniform i)) = "BoolUniform[" ++ show i ++ "]"

nodeLabel (DFBool (DFBoolCond _ _ _)) = "BoolCond"

nodeLabel (DFBool (DFBoolLessThan _ _)) = "LessThan"
nodeLabel (DFBool (DFBoolLessThanEqual _ _)) = "LessThanEqual"
nodeLabel (DFBool (DFBoolGreaterThan _ _)) = "GreaterThan"
nodeLabel (DFBool (DFBoolGreaterThanEqual _ _)) = "GreaterThanEqual"

nodeLabel (DFBool (DFBoolEqualReal _ _)) = "EqualReal"
nodeLabel (DFBool (DFBoolNotEqualReal _ _)) = "NotEqualReal"
nodeLabel (DFBool (DFBoolEqualBool _ _)) = "EqualBool"
nodeLabel (DFBool (DFBoolNotEqualBool _ _)) = "NotEqualBool"

nodeLabel (DFBool (DFBoolAnd _ _)) = "And"
nodeLabel (DFBool (DFBoolOr _ _)) = "Or"
nodeLabel (DFBool (DFBoolNot _)) = "Not"

nodeLabel (DFSample (DFSample1D i _)) = "Texture[" ++ show i ++ ", 1D]"
nodeLabel (DFSample (DFSample2D i _ _)) = "Texture[" ++ show i ++ ", 2D]"
nodeLabel (DFSample (DFSample3D i _ _ _)) = "Texture[" ++ show i ++ ", 3D]"
nodeLabel (DFSample (DFSampleCube i _ _ _)) = "Texture[" ++ show i ++ ", Cube]"
