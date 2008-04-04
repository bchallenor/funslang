module Dataflow(dependencyGraph, graphvizCompile, nodeID) where

import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Array
import Data.Graph
import System.IO

import Representation


-- Dumps a GraphViz "dot" representation of a graph to a file.
graphvizCompile :: DFGraph -> String -> IO ()
graphvizCompile g filename = do
  let src = graphviz g
  writeFile filename src


-- Return a GraphViz "dot" representation of a graph.
graphviz :: DFGraph -> String
graphviz (adjs, _, mvn) =
  let assoclist = assocs adjs in
    "digraph DF {"
    ++
    (concat $ map (\(v,_) -> case IntMap.lookup v mvn of { Just n -> "\nn" ++ show v ++ " [label=\"" ++ (nodeLabel n) ++ "\", color=\"#" ++ case n of { DFReal _ -> "800000"; DFBool _ -> "000080"; DFSample _ -> "008000"; DFTex _ -> "800080" } ++ "\"];"; Nothing -> "\n// node " ++ show v ++ " is not required" }) assoclist)
    ++
    (concat $ map (\(v,vs) -> concat $ map (\v' -> "\nn" ++ show v ++ " -> n" ++ show v' ++ ";") vs) $ assoclist)
    ++
    "\n}"


-- Given a Value, makes its DFGraph.
-- Note that the arcs in the Graph representation are necessarily the transpose of the DF representation.
dependencyGraph :: Value -> InterpretState -> DFGraph
dependencyGraph value s =
  let result_ns = getResultDFs value in
  let (es, mvn) = dependencyEdges ([], IntMap.empty) result_ns in
    (buildG (0, num_nodes s - 1) es, result_ns, mvn)

-- Process a list of DFs in the same context.
dependencyEdges :: ([Edge], IntMap.IntMap DF) -> [DF] -> ([Edge], IntMap.IntMap DF)
dependencyEdges acc [] = acc
dependencyEdges acc@(_, mvn) (n:ns) =
  let v = nodeID n in
  case IntMap.lookup v mvn of
    Just _ ->
      -- Next please!
      dependencyEdges acc ns
    Nothing ->
      -- We haven't been here before.
      -- Recurse on dependencies.
      let ndeps = nodeDependencies n in
      let (es', mvn') = dependencyEdges acc ndeps in
      
      -- Look up vdep for each dependency ndep and add an edge from the vdep to v.
      let es''' = List.foldl' (\es'' ndep -> let vdep = nodeID ndep in (vdep, v):es'') es' ndeps in
      
      -- Finally, add this node to the accumulator and then recurse.
      dependencyEdges (es''', IntMap.insert v n mvn') ns


-- Gets the result DFs which represent this Value.
getResultDFs :: Value -> [DF]
getResultDFs (ValueUnit) = []
getResultDFs (ValueReal df) = [DFReal df]
getResultDFs (ValueBool df) = [DFBool df]
getResultDFs (ValueTex df) = [DFTex df]
getResultDFs (ValueArray vs) = concat $ map getResultDFs vs
getResultDFs (ValueTuple vs) = concat $ map getResultDFs vs
getResultDFs (ValueFun _) = error "function values cannot be represented by a dataflow graph"


-- Given a node, returns the list of nodes that it depends on.
nodeDependencies :: DF -> [DF]

nodeDependencies (DFReal (DFRealLiteral _ _)) = []
nodeDependencies (DFReal (DFRealVarying _ _)) = []
nodeDependencies (DFReal (DFRealUniform _ _)) = []

nodeDependencies (DFReal (DFRealCond _ dfbc dfr1 dfr2)) = [DFBool dfbc, DFReal dfr1, DFReal dfr2]

nodeDependencies (DFReal (DFRealAdd _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealSub _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealMul _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealDiv _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealNeg _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealRcp _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealRsq _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealAbs _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealMin _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealMax _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealFloor _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealCeiling _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealRound _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealTruncate _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealFract _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealExp _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealExp2 _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealLog _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealLog2 _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealPow _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFReal (DFRealSin _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealCos _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealTan _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealASin _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealACos _ dfr1)) = [DFReal dfr1]
nodeDependencies (DFReal (DFRealATan _ dfr1)) = [DFReal dfr1]

nodeDependencies (DFReal (DFRealChannelR _ dfs1)) = [DFSample dfs1]
nodeDependencies (DFReal (DFRealChannelG _ dfs1)) = [DFSample dfs1]
nodeDependencies (DFReal (DFRealChannelB _ dfs1)) = [DFSample dfs1]
nodeDependencies (DFReal (DFRealChannelA _ dfs1)) = [DFSample dfs1]

nodeDependencies (DFBool (DFBoolLiteral _ _)) = []
nodeDependencies (DFBool (DFBoolVarying _ _)) = []
nodeDependencies (DFBool (DFBoolUniform _ _)) = []

nodeDependencies (DFBool (DFBoolCond _ dfbc dfb1 dfb2)) = [DFBool dfbc, DFBool dfb1, DFBool dfb2]

nodeDependencies (DFBool (DFBoolLessThan _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFBool (DFBoolLessThanEqual _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFBool (DFBoolGreaterThan _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFBool (DFBoolGreaterThanEqual _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]

nodeDependencies (DFBool (DFBoolEqualReal _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFBool (DFBoolNotEqualReal _ dfr1 dfr2)) = [DFReal dfr1, DFReal dfr2]
nodeDependencies (DFBool (DFBoolEqualBool _ dfb1 dfb2)) = [DFBool dfb1, DFBool dfb2]
nodeDependencies (DFBool (DFBoolNotEqualBool _ dfb1 dfb2)) = [DFBool dfb1, DFBool dfb2]
nodeDependencies (DFBool (DFBoolEqualTex _ dft1 dft2)) = [DFTex dft1, DFTex dft2]
nodeDependencies (DFBool (DFBoolNotEqualTex _ dft1 dft2)) = [DFTex dft1, DFTex dft2]

nodeDependencies (DFBool (DFBoolAnd _ dfb1 dfb2)) = [DFBool dfb1, DFBool dfb2]
nodeDependencies (DFBool (DFBoolOr _ dfb1 dfb2)) = [DFBool dfb1, DFBool dfb2]
nodeDependencies (DFBool (DFBoolNot _ dfb1)) = [DFBool dfb1]

nodeDependencies (DFTex (DFTexConstant _ _ _)) = []
nodeDependencies (DFTex (DFTexCond _ dfbc dft1 dft2)) = [DFBool dfbc, DFTex dft1, DFTex dft2]

nodeDependencies (DFSample (DFSampleTex _ dft dfrs)) = DFTex dft : map DFReal dfrs


-- Given a node, returns a pretty label for visualization.
nodeLabel :: DF -> String

nodeLabel (DFReal (DFRealLiteral _ d)) = show d
nodeLabel (DFReal (DFRealVarying _ i)) = "RealVarying[" ++ show i ++ "]"
nodeLabel (DFReal (DFRealUniform _ i)) = "RealUniform[" ++ show i ++ "]"

nodeLabel (DFReal (DFRealCond _ _ _ _)) = "Cond"

nodeLabel (DFReal (DFRealAdd _ _ _)) = "Add"
nodeLabel (DFReal (DFRealSub _ _ _)) = "Sub"
nodeLabel (DFReal (DFRealMul _ _ _)) = "Mul"
nodeLabel (DFReal (DFRealDiv _ _ _)) = "Div"
nodeLabel (DFReal (DFRealNeg _ _)) = "Neg"
nodeLabel (DFReal (DFRealRcp _ _)) = "Rcp"
nodeLabel (DFReal (DFRealRsq _ _)) = "Rsq"
nodeLabel (DFReal (DFRealAbs _ _)) = "Abs"
nodeLabel (DFReal (DFRealMin _ _ _)) = "Min"
nodeLabel (DFReal (DFRealMax _ _ _)) = "Max"
nodeLabel (DFReal (DFRealFloor _ _)) = "Floor"
nodeLabel (DFReal (DFRealCeiling _ _)) = "Ceiling"
nodeLabel (DFReal (DFRealRound _ _)) = "Round"
nodeLabel (DFReal (DFRealTruncate _ _)) = "Truncate"
nodeLabel (DFReal (DFRealFract _ _)) = "Fract"
nodeLabel (DFReal (DFRealExp _ _)) = "Exp"
nodeLabel (DFReal (DFRealExp2 _ _)) = "Exp2"
nodeLabel (DFReal (DFRealLog _ _)) = "Log"
nodeLabel (DFReal (DFRealLog2 _ _)) = "Log2"
nodeLabel (DFReal (DFRealPow _ _ _)) = "Pow"
nodeLabel (DFReal (DFRealSin _ _)) = "Sin"
nodeLabel (DFReal (DFRealCos _ _)) = "Cos"
nodeLabel (DFReal (DFRealTan _ _)) = "Tan"
nodeLabel (DFReal (DFRealASin _ _)) = "ASin"
nodeLabel (DFReal (DFRealACos _ _)) = "ACos"
nodeLabel (DFReal (DFRealATan _ _)) = "ATan"

nodeLabel (DFReal (DFRealChannelR _ _)) = "ChannelR"
nodeLabel (DFReal (DFRealChannelG _ _)) = "ChannelG"
nodeLabel (DFReal (DFRealChannelB _ _)) = "ChannelB"
nodeLabel (DFReal (DFRealChannelA _ _)) = "ChannelA"

nodeLabel (DFBool (DFBoolLiteral _ b)) = show b
nodeLabel (DFBool (DFBoolVarying _ i)) = "BoolVarying[" ++ show i ++ "]"
nodeLabel (DFBool (DFBoolUniform _ i)) = "BoolUniform[" ++ show i ++ "]"

nodeLabel (DFBool (DFBoolCond _ _ _ _)) = "BoolCond"

nodeLabel (DFBool (DFBoolLessThan _ _ _)) = "LessThan"
nodeLabel (DFBool (DFBoolLessThanEqual _ _ _)) = "LessThanEqual"
nodeLabel (DFBool (DFBoolGreaterThan _ _ _)) = "GreaterThan"
nodeLabel (DFBool (DFBoolGreaterThanEqual _ _ _)) = "GreaterThanEqual"

nodeLabel (DFBool (DFBoolEqualReal _ _ _)) = "EqualReal"
nodeLabel (DFBool (DFBoolNotEqualReal _ _ _)) = "NotEqualReal"
nodeLabel (DFBool (DFBoolEqualBool _ _ _)) = "EqualBool"
nodeLabel (DFBool (DFBoolNotEqualBool _ _ _)) = "NotEqualBool"
nodeLabel (DFBool (DFBoolEqualTex _ _ _)) = "EqualTex"
nodeLabel (DFBool (DFBoolNotEqualTex _ _ _)) = "NotEqualTex"

nodeLabel (DFBool (DFBoolAnd _ _ _)) = "And"
nodeLabel (DFBool (DFBoolOr _ _ _)) = "Or"
nodeLabel (DFBool (DFBoolNot _ _)) = "Not"

nodeLabel (DFTex (DFTexConstant _ tk i)) = "Texture[" ++ show i ++ ", " ++ show tk ++ "]"
nodeLabel (DFTex (DFTexCond _ _ _ _)) = "TexCond"

nodeLabel (DFSample (DFSampleTex _ dft _)) = "Sample" ++ show (getTexKindOfDFTex dft)


-- Given a node, returns its ID.
nodeID :: DF -> DFID

nodeID (DFReal (DFRealLiteral n _)) = n
nodeID (DFReal (DFRealVarying n _)) = n
nodeID (DFReal (DFRealUniform n _)) = n

nodeID (DFReal (DFRealCond n _ _ _)) = n

nodeID (DFReal (DFRealAdd n _ _)) = n
nodeID (DFReal (DFRealSub n _ _)) = n
nodeID (DFReal (DFRealMul n _ _)) = n
nodeID (DFReal (DFRealDiv n _ _)) = n
nodeID (DFReal (DFRealNeg n _)) = n
nodeID (DFReal (DFRealRcp n _)) = n
nodeID (DFReal (DFRealRsq n _)) = n
nodeID (DFReal (DFRealAbs n _)) = n
nodeID (DFReal (DFRealMin n _ _)) = n
nodeID (DFReal (DFRealMax n _ _)) = n
nodeID (DFReal (DFRealFloor n _)) = n
nodeID (DFReal (DFRealCeiling n _)) = n
nodeID (DFReal (DFRealRound n _)) = n
nodeID (DFReal (DFRealTruncate n _)) = n
nodeID (DFReal (DFRealFract n _)) = n
nodeID (DFReal (DFRealExp n _)) = n
nodeID (DFReal (DFRealExp2 n _)) = n
nodeID (DFReal (DFRealLog n _)) = n
nodeID (DFReal (DFRealLog2 n _)) = n
nodeID (DFReal (DFRealPow n _ _)) = n
nodeID (DFReal (DFRealSin n _)) = n
nodeID (DFReal (DFRealCos n _)) = n
nodeID (DFReal (DFRealTan n _)) = n
nodeID (DFReal (DFRealASin n _)) = n
nodeID (DFReal (DFRealACos n _)) = n
nodeID (DFReal (DFRealATan n _)) = n

nodeID (DFReal (DFRealChannelR n _)) = n
nodeID (DFReal (DFRealChannelG n _)) = n
nodeID (DFReal (DFRealChannelB n _)) = n
nodeID (DFReal (DFRealChannelA n _)) = n

nodeID (DFBool (DFBoolLiteral n _)) = n
nodeID (DFBool (DFBoolVarying n _)) = n
nodeID (DFBool (DFBoolUniform n _)) = n

nodeID (DFBool (DFBoolCond n _ _ _)) = n

nodeID (DFBool (DFBoolLessThan n _ _)) = n
nodeID (DFBool (DFBoolLessThanEqual n _ _)) = n
nodeID (DFBool (DFBoolGreaterThan n _ _)) = n
nodeID (DFBool (DFBoolGreaterThanEqual n _ _)) = n

nodeID (DFBool (DFBoolEqualReal n _ _)) = n
nodeID (DFBool (DFBoolNotEqualReal n _ _)) = n
nodeID (DFBool (DFBoolEqualBool n _ _)) = n
nodeID (DFBool (DFBoolNotEqualBool n _ _)) = n
nodeID (DFBool (DFBoolEqualTex n _ _)) = n
nodeID (DFBool (DFBoolNotEqualTex n _ _)) = n

nodeID (DFBool (DFBoolAnd n _ _)) = n
nodeID (DFBool (DFBoolOr n _ _)) = n
nodeID (DFBool (DFBoolNot n _)) = n

nodeID (DFTex (DFTexConstant n _ _)) = n
nodeID (DFTex (DFTexCond n _ _ _)) = n

nodeID (DFSample (DFSampleTex n _ _)) = n
