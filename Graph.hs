module Graph
  ( -- * Graphs
    Graph
  , Colour(..)
  , Label(..)
  , fromList
  -- * Edges
  , edgeFromTo
  , claimEdge
  , loseEdge
  -- * Paths
  , shortestPath
  ) where

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Tree as G
import qualified Data.Graph.Inductive.Query.SP as G
import Data.Maybe (fromJust, listToMaybe, mapMaybe)

-------------------------------------------------------------------------------
-- Graphs

-- | The world map is represented as a weighted graph, where nodes are
-- places and edge weights correspond to the number of tickets to buy
-- it.
newtype Graph a = Graph { graph :: G.Gr a Label }
  deriving (Eq, Show)

-- | All the possible route colours.
data Colour = Pink | White | Blue | Yellow | Orange | Black | Red | Green
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | A label describes a single-hop route between two destinations.
data Label = Label
    { lcolour :: [Maybe Colour]
      -- ^ The colours of the route. @Nothing@ indicates a \"grey\"
      -- route: any colour can be used to build it.
    , llocos  :: Int
      -- ^ The number of required locomotives.
    , lweight :: Int
      -- ^ The total number of required cards.
    }
  deriving Show

-- | Equality is modulo colours.
instance Eq Label where
  lbl1 == lbl2 = (lweight lbl1, llocos lbl1) == (lweight lbl2, llocos lbl2)

instance Ord Label where
  lbl1 <= lbl2 = (lweight lbl1, llocos lbl1) <= (lweight lbl2, llocos lbl2)

-- | Numeric operations only touch the weight.
instance Num Label where
  (+) = combinelbls (+)
  (*) = combinelbls (*)
  (-) = combinelbls (-)
  negate = lblmap negate
  abs    = lblmap abs
  signum = lblmap signum
  fromInteger = Label [] 0 . fromInteger

instance Real Label where
  toRational = toRational . lweight

-- | Convert a list of edges into a graph.
fromList :: Enum a => [(a, a, Label)] -> Graph a
fromList weights = Graph (G.mkGraph nodes edges) where
  nodes = [(n, toEnum n) | (n, _, _) <- edges]
  edges = [(fromEnum n1, fromEnum n2, l) | (n1, n2, l) <- weights, not . null $ lcolour l] ++
          [(fromEnum n2, fromEnum n1, l) | (n1, n2, l) <- weights, not . null $ lcolour l]


-------------------------------------------------------------------------------
-- Edges

-- | Get edge label between the given pair of nodes.
edgeFromTo :: Enum a => a -> a -> Graph a -> Maybe Label
edgeFromTo from to gr = listToMaybe . mapMaybe go $ G.lsuc (graph gr) (fromEnum from) where
  go (n, lbl) | n == fromEnum to = Just lbl
              | otherwise = Nothing

-- | Claim ownership of an edge. This reduces its cost to zero.
--
-- Representational issue: weights share shared between colours, so
-- claiming one edge will claim all the parallel ones. However, the
-- game rules forbid buying multiple parallel edges, so this may not
-- be a problem in practice.
claimEdge :: Enum a => a -> a -> Maybe Colour -> Graph a -> Graph a
claimEdge = modlabel claim where
  claim l = Just l { lweight = 0 }

-- | Remove an edge from the graph. This is a simple way of indicating
-- that someone else has claimed it.
loseEdge :: Enum a => a -> a -> Maybe Colour -> Graph a -> Graph a
loseEdge from to colour = modlabel lose from to colour where
  lose l = Just l { lcolour = filter (/=colour) (lcolour l) }


-------------------------------------------------------------------------------
-- Paths

-- | Get the shortest path between two nodes, if one exists. If two
-- paths have the same weight, but one has fewer edges, that one will
-- be preferred.
shortestPath :: Enum a => a -> a -> Graph a -> [(a, a, Label)]
shortestPath from to gr = go $ G.sp (fromEnum from) (fromEnum to) (graph gr2) where
  go (n1:n2:rest) = (toEnum n1, toEnum n2, edgelbl n1 n2) : go (n2:rest)
  go _ = []

  -- this use of 'fromJust' is safe, because 'G.sp' only produces a
  -- valid path.
  edgelbl n1 n2 = fromJust (edgeFromTo (toEnum n1) (toEnum n2) gr)

  -- construct a new graph which is the same, but all edge weights
  -- have 1 added. This means that a single n-cost edge will be
  -- preferred over a multi-edge path summing to n.
  gr2 = modgraph (\(n1, n2, l) -> Just (n1, n2, l+1)) gr


-------------------------------------------------------------------------------
-- Internal

-- | Apply a function to the weight of a label.
lblmap :: (Int -> Int) -> Label -> Label
lblmap f lbl = lbl { lweight = f (lweight lbl) }

-- | Combine two labels with the given weight function.
combinelbls :: (Int -> Int -> Int) -> Label -> Label -> Label
combinelbls f lbl1 lbl2 = Label
  { lcolour = lcolour lbl1 ++ lcolour lbl2
  , llocos  = llocos lbl1 `max` llocos lbl2
  , lweight = lweight lbl1 `f` lweight lbl2
  }

-- | Construct a new graph by mapping/filtering the edges.
modgraph :: Enum a => ((a, a, Label) -> Maybe (a, a, Label)) -> Graph a -> Graph a
modgraph f gr = fromList (mapMaybe f edges) where
  edges = [(toEnum n1, toEnum n2, l) | (n1, n2, l) <- G.labEdges (graph gr), n1 < n2]

-- | Modify a single edge in a graph.
modlabel :: Enum a => (Label -> Maybe Label) -> a -> a -> Maybe Colour -> Graph a -> Graph a
modlabel f from to colour = modgraph go where
  go (n1, n2, l)
    | colour `elem` lcolour l && (check n1 n2 || check n2 n1)
      = (\l' -> (n1, n2, l')) <$> f l
    | otherwise = Just (n1, n2, l)

  check n1 n2 = fromEnum n1 == fromEnum from && fromEnum n2 == fromEnum to
