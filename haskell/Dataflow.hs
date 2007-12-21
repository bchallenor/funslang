module Dataflow where

import qualified Data.Map as Map
import Data.Graph

import Representation


-- Given a list of roots (i.e. the values we wish to calculate) of a DFNode graph,
-- makes a Graph representing its dependencies.
-- Each edge (a,b) in the Graph means that b depends on a, or equivalently
-- that a must be calculated before b.
-- This has the effect of (1) tranposing the DFNode graph (where the edges were
-- in the other direction) and (2) removing any common subgraphs.
-- dependencyEdges :: [DFNode] -> (Graph, Map DFNode Vertex, Map Vertex DFNode)
dependencyGraph :: [DFNode] -> (Graph, Map.Map DFNode Vertex, Map.Map Vertex DFNode)
dependencyGraph ns =
  let (v', es', mnv', mvn') = dependencyEdges (0, [], Map.empty, Map.empty) ns in
  (buildG (0, v'-1) es', mnv', mvn')

-- (next Vertex available for use, edges so far, map so far, map so far)
type DependencyEdgesAcc = (Vertex, [Edge], Map.Map DFNode Vertex, Map.Map Vertex DFNode)

dependencyEdges :: DependencyEdgesAcc -> [DFNode] -> DependencyEdgesAcc
dependencyEdges acc [] = acc
dependencyEdges (v, es, mnv, mvn) (n:ns) =
  -- process this node
  let (v', es', mnv', mvn') = dependencyEdges' (v, es, mnv, mvn) n (getDependencies n) in
  dependencyEdges (v', es', mnv', mvn') ns

dependencyEdges' :: DependencyEdgesAcc -> DFNode -> [DFNode] -> DependencyEdgesAcc
dependencyEdges' (v, es, mnv, mvn) n [] = (v+1, es, Map.insert n v mnv, Map.insert v n mvn)
dependencyEdges' (v, es, mnv, mvn) n (depn:depns) =
  -- is depn already in the graph?
  case Map.lookup depn mnv of
    Just depv -> -- yes, so add this edges to the graph and recurse on the rest of the dependencies of n
      dependencyEdges' (v, (depv, v):es, mnv, mvn) n depns
    Nothing -> -- no, so process depn first, then recurse again on n (and this time we'll succeed)
      let (v', es', mnv', mvn') = dependencyEdges (v, es, mnv, mvn) [depn] in
      dependencyEdges' (v', es', mnv', mvn') n (depn:depns)


-- Given a node, returns the list of nodes that it depends on.
getDependencies :: DFNode -> [DFNode]

getDependencies (DFNodeReal (DFRealLiteral _)) = []
getDependencies (DFNodeReal (DFRealVarying _)) = []
getDependencies (DFNodeReal (DFRealUniform _)) = []

getDependencies (DFNodeReal (DFRealCond dfbc dfr1 dfr2)) = [DFNodeBool dfbc, DFNodeReal dfr1, DFNodeReal dfr2]

getDependencies (DFNodeReal (DFRealAdd dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeReal (DFRealSub dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeReal (DFRealMul dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeReal (DFRealDiv dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeReal (DFRealNeg dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealRcp dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealRsq dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealAbs dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealMin dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeReal (DFRealMax dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeReal (DFRealFloor dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealCeiling dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealRound dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealTruncate dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealFract dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealExp dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealExp2 dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealLog dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealLog2 dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealPow dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeReal (DFRealSin dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealCos dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealTan dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealASin dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealACos dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeReal (DFRealATan dfr1)) = [DFNodeReal dfr1]

getDependencies (DFNodeReal (DFRealGetTexR dfs1)) = [DFNodeSample dfs1]
getDependencies (DFNodeReal (DFRealGetTexG dfs1)) = [DFNodeSample dfs1]
getDependencies (DFNodeReal (DFRealGetTexB dfs1)) = [DFNodeSample dfs1]
getDependencies (DFNodeReal (DFRealGetTexA dfs1)) = [DFNodeSample dfs1]

getDependencies (DFNodeBool (DFBoolLiteral _)) = []
getDependencies (DFNodeBool (DFBoolVarying _)) = []
getDependencies (DFNodeBool (DFBoolUniform _)) = []

getDependencies (DFNodeBool (DFBoolCond dfbc dfb1 dfb2)) = [DFNodeBool dfbc, DFNodeBool dfb1, DFNodeBool dfb2]

getDependencies (DFNodeBool (DFBoolLessThan dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeBool (DFBoolLessThanEqual dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeBool (DFBoolGreaterThan dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeBool (DFBoolGreaterThanEqual dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]

getDependencies (DFNodeBool (DFBoolEqualReal dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeBool (DFBoolNotEqualReal dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeBool (DFBoolEqualBool dfb1 dfb2)) = [DFNodeBool dfb1, DFNodeBool dfb2]
getDependencies (DFNodeBool (DFBoolNotEqualBool dfb1 dfb2)) = [DFNodeBool dfb1, DFNodeBool dfb2]

getDependencies (DFNodeBool (DFBoolAnd dfb1 dfb2)) = [DFNodeBool dfb1, DFNodeBool dfb2]
getDependencies (DFNodeBool (DFBoolOr dfb1 dfb2)) = [DFNodeBool dfb1, DFNodeBool dfb2]
getDependencies (DFNodeBool (DFBoolNot dfb1)) = [DFNodeBool dfb1]

getDependencies (DFNodeSample (DFSample1D _ dfr1)) = [DFNodeReal dfr1]
getDependencies (DFNodeSample (DFSample2D _ dfr1 dfr2)) = [DFNodeReal dfr1, DFNodeReal dfr2]
getDependencies (DFNodeSample (DFSample3D _ dfr1 dfr2 dfr3)) = [DFNodeReal dfr1, DFNodeReal dfr2, DFNodeReal dfr3]
getDependencies (DFNodeSample (DFSampleCube _ dfr1 dfr2 dfr3)) = [DFNodeReal dfr1, DFNodeReal dfr2, DFNodeReal dfr3]
