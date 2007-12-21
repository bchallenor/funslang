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
type DFGraph = (Graph, Map.Map DFNode Vertex, Map.Map Vertex DFNode)


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
graphviz (adjs, _, mvn) =
  let assoclist = assocs adjs in
    "digraph DF {"
    ++
    (concat $ map (\(v,_) -> case Map.lookup v mvn of { Just n -> "\nn" ++ show v ++ " [label=\"" ++ (nodeLabel n) ++ "\", color=\"#" ++ case n of { DFNodeReal _ -> "800000"; DFNodeBool _ -> "000080"; DFNodeSample _ -> "008000" } ++ "\"];"; Nothing -> error "" }) assoclist)
    ++
    (concat $ map (\(v,vs) -> concat $ map (\v' -> "\nn" ++ show v ++ " -> n" ++ show v' ++ ";") vs) $ assoclist)
    ++
    "\n}"


-- Given a Value, makes its DFGraph.
-- This has the effect of (1) transposing the DFNode graph (where the edges were
-- in the other direction) and (2) removing any common subgraphs.
dependencyGraph :: Value -> DFGraph
dependencyGraph value =
  let (v', es', mnv', mvn') = dependencyEdges (0, [], Map.empty, Map.empty) (getRootDFNodes value) in
  (buildG (0, v'-1) es', mnv', mvn')

-- (next Vertex available for use, edges so far, map so far, map so far)
type DependencyEdgesAcc = (Vertex, [Edge], Map.Map DFNode Vertex, Map.Map Vertex DFNode)

-- Process a list of DFNodes in the same context.
dependencyEdges :: DependencyEdgesAcc -> [DFNode] -> DependencyEdgesAcc
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


-- Gets the root DFNodes which represent this Value.
getRootDFNodes :: Value -> [DFNode]
getRootDFNodes (ValueUnit) = error getRootDFNodesErrorMsg
getRootDFNodes (ValueDFReal df) = [DFNodeReal df]
getRootDFNodes (ValueDFBool df) = [DFNodeBool df]
getRootDFNodes (ValueTexture1D _) = error getRootDFNodesErrorMsg
getRootDFNodes (ValueTexture2D _) = error getRootDFNodesErrorMsg
getRootDFNodes (ValueTexture3D _) = error getRootDFNodesErrorMsg
getRootDFNodes (ValueTextureCube _) = error getRootDFNodesErrorMsg
getRootDFNodes (ValueArray vs) = concat $ map getRootDFNodes vs
getRootDFNodes (ValueTuple vs) = concat $ map getRootDFNodes vs
getRootDFNodes (ValueFun _) = error getRootDFNodesErrorMsg
getRootDFNodesErrorMsg :: String
getRootDFNodesErrorMsg = "this value cannot be represented by a dataflow graph"


-- Given a node, returns the list of nodes that it depends on.
nodeDependencies :: DFNode -> [DFNode]

nodeDependencies (DFNodeReal (DFRealLiteral _)) = []
nodeDependencies (DFNodeReal (DFRealVarying _)) = []
nodeDependencies (DFNodeReal (DFRealUniform _)) = []

nodeDependencies (DFNodeReal (DFRealCond dfbc dfr1 dfr2)) = [DFNodeBool dfbc, DFNodeReal dfr1, DFNodeReal dfr2]

nodeDependencies (DFNodeReal (DFRealAdd dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeReal (DFRealSub dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeReal (DFRealMul dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeReal (DFRealDiv dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeReal (DFRealNeg dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealRcp dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealRsq dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealAbs dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealMin dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeReal (DFRealMax dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeReal (DFRealFloor dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealCeiling dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealRound dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealTruncate dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealFract dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealExp dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealExp2 dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealLog dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealLog2 dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealPow dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeReal (DFRealSin dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealCos dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealTan dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealASin dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealACos dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeReal (DFRealATan dfr1)) = [DFNodeReal dfr1]

nodeDependencies (DFNodeReal (DFRealGetTexR dfs1)) = [DFNodeSample dfs1]
nodeDependencies (DFNodeReal (DFRealGetTexG dfs1)) = [DFNodeSample dfs1]
nodeDependencies (DFNodeReal (DFRealGetTexB dfs1)) = [DFNodeSample dfs1]
nodeDependencies (DFNodeReal (DFRealGetTexA dfs1)) = [DFNodeSample dfs1]

nodeDependencies (DFNodeBool (DFBoolLiteral _)) = []
nodeDependencies (DFNodeBool (DFBoolVarying _)) = []
nodeDependencies (DFNodeBool (DFBoolUniform _)) = []

nodeDependencies (DFNodeBool (DFBoolCond dfbc dfb1 dfb2)) = [DFNodeBool dfbc, DFNodeBool dfb1, DFNodeBool dfb2]

nodeDependencies (DFNodeBool (DFBoolLessThan dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeBool (DFBoolLessThanEqual dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeBool (DFBoolGreaterThan dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeBool (DFBoolGreaterThanEqual dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]

nodeDependencies (DFNodeBool (DFBoolEqualReal dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeBool (DFBoolNotEqualReal dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeBool (DFBoolEqualBool dfb1 dfb2)) = [DFNodeBool dfb1, DFNodeBool dfb2]
nodeDependencies (DFNodeBool (DFBoolNotEqualBool dfb1 dfb2)) = [DFNodeBool dfb1, DFNodeBool dfb2]

nodeDependencies (DFNodeBool (DFBoolAnd dfb1 dfb2)) = [DFNodeBool dfb1, DFNodeBool dfb2]
nodeDependencies (DFNodeBool (DFBoolOr dfb1 dfb2)) = [DFNodeBool dfb1, DFNodeBool dfb2]
nodeDependencies (DFNodeBool (DFBoolNot dfb1)) = [DFNodeBool dfb1]

nodeDependencies (DFNodeSample (DFSample1D _ dfr1)) = [DFNodeReal dfr1]
nodeDependencies (DFNodeSample (DFSample2D _ dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
nodeDependencies (DFNodeSample (DFSample3D _ dfr1 dfr2 dfr3)) = [DFNodeReal dfr1, DFNodeReal dfr2, DFNodeReal dfr3]
nodeDependencies (DFNodeSample (DFSampleCube _ dfr1 dfr2 dfr3)) = [DFNodeReal dfr1, DFNodeReal dfr2, DFNodeReal dfr3]


-- Given a node, returns a pretty label for visualization.
nodeLabel :: DFNode -> String

nodeLabel (DFNodeReal (DFRealLiteral d)) = show d
nodeLabel (DFNodeReal (DFRealVarying i)) = "Varying[" ++ show i ++ "]"
nodeLabel (DFNodeReal (DFRealUniform i)) = "Uniform[" ++ show i ++ "]"

nodeLabel (DFNodeReal (DFRealCond _ _ _)) = "Cond"

nodeLabel (DFNodeReal (DFRealAdd _ _)) = "Add"
nodeLabel (DFNodeReal (DFRealSub _ _)) = "Sub"
nodeLabel (DFNodeReal (DFRealMul _ _)) = "Mul"
nodeLabel (DFNodeReal (DFRealDiv _ _)) = "Div"
nodeLabel (DFNodeReal (DFRealNeg _)) = "Neg"
nodeLabel (DFNodeReal (DFRealRcp _)) = "Rcp"
nodeLabel (DFNodeReal (DFRealRsq _)) = "Rsq"
nodeLabel (DFNodeReal (DFRealAbs _)) = "Abs"
nodeLabel (DFNodeReal (DFRealMin _ _)) = "Min"
nodeLabel (DFNodeReal (DFRealMax _ _)) = "Max"
nodeLabel (DFNodeReal (DFRealFloor _)) = "Floor"
nodeLabel (DFNodeReal (DFRealCeiling _)) = "Ceiling"
nodeLabel (DFNodeReal (DFRealRound _)) = "Round"
nodeLabel (DFNodeReal (DFRealTruncate _)) = "Truncate"
nodeLabel (DFNodeReal (DFRealFract _)) = "Fract"
nodeLabel (DFNodeReal (DFRealExp _)) = "Exp"
nodeLabel (DFNodeReal (DFRealExp2 _)) = "Exp2"
nodeLabel (DFNodeReal (DFRealLog _)) = "Log"
nodeLabel (DFNodeReal (DFRealLog2 _)) = "Log2"
nodeLabel (DFNodeReal (DFRealPow _ _)) = "Pow"
nodeLabel (DFNodeReal (DFRealSin _)) = "Sin"
nodeLabel (DFNodeReal (DFRealCos _)) = "Cos"
nodeLabel (DFNodeReal (DFRealTan _)) = "Tan"
nodeLabel (DFNodeReal (DFRealASin _)) = "ASin"
nodeLabel (DFNodeReal (DFRealACos _)) = "ACos"
nodeLabel (DFNodeReal (DFRealATan _)) = "ATan"

nodeLabel (DFNodeReal (DFRealGetTexR _)) = "GetTexR"
nodeLabel (DFNodeReal (DFRealGetTexG _)) = "GetTexG"
nodeLabel (DFNodeReal (DFRealGetTexB _)) = "GetTexB"
nodeLabel (DFNodeReal (DFRealGetTexA _)) = "GetTexA"

nodeLabel (DFNodeBool (DFBoolLiteral b)) = show b
nodeLabel (DFNodeBool (DFBoolVarying i)) = "Varying[" ++ show i ++ "]"
nodeLabel (DFNodeBool (DFBoolUniform i)) = "Uniform[" ++ show i ++ "]"

nodeLabel (DFNodeBool (DFBoolCond _ _ _)) = "BoolCond"

nodeLabel (DFNodeBool (DFBoolLessThan _ _)) = "LessThan"
nodeLabel (DFNodeBool (DFBoolLessThanEqual _ _)) = "LessThanEqual"
nodeLabel (DFNodeBool (DFBoolGreaterThan _ _)) = "GreaterThan"
nodeLabel (DFNodeBool (DFBoolGreaterThanEqual _ _)) = "GreaterThanEqual"

nodeLabel (DFNodeBool (DFBoolEqualReal _ _)) = "EqualReal"
nodeLabel (DFNodeBool (DFBoolNotEqualReal _ _)) = "NotEqualReal"
nodeLabel (DFNodeBool (DFBoolEqualBool _ _)) = "EqualBool"
nodeLabel (DFNodeBool (DFBoolNotEqualBool _ _)) = "NotEqualBool"

nodeLabel (DFNodeBool (DFBoolAnd _ _)) = "And"
nodeLabel (DFNodeBool (DFBoolOr _ _)) = "Or"
nodeLabel (DFNodeBool (DFBoolNot _)) = "Not"

nodeLabel (DFNodeSample (DFSample1D i _)) = "Texture[" ++ show i ++ ", 1D]"
nodeLabel (DFNodeSample (DFSample2D i _ _)) = "Texture[" ++ show i ++ ", 2D]"
nodeLabel (DFNodeSample (DFSample3D i _ _ _)) = "Texture[" ++ show i ++ ", 3D]"
nodeLabel (DFNodeSample (DFSampleCube i _ _ _)) = "Texture[" ++ show i ++ ", Cube]"
