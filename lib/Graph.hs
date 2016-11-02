module Graph
  ( -- * Graphs
    Graph
  , Colour(..)
  , Label(..)
  , fromList
  , toList
  -- * Nodes
  , neighbours
  -- * Edges
  , edgeFromTo
  , claimEdge
  , claimedEdges
  , loseEdge
  -- * Paths
  , shortestPath
  , pathCost
  , pathScore
  ) where

import Data.Function (on)
import Data.List (partition)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Tree as G
import qualified Data.Graph.Inductive.Query.SP as G
import Data.Maybe (fromJust, listToMaybe, mapMaybe)

import Util

-------------------------------------------------------------------------------
-- Graphs

-- | The world map is represented as a weighted graph, where nodes are
-- places and edge weights correspond to the number of tickets to buy
-- it.
newtype Graph a = Graph { graph :: G.Gr a Label }
  deriving Show

instance Eq (Graph a) where
  (==) = (==) `on` G.labEdges . graph

-- | All the possible route and card colours. The @Special@ colour
-- refers to both locomotives and grey routes.
data Colour = Pink | White | Blue | Yellow | Orange | Black | Red | Green | Special
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | A label describes a single-hop route between two destinations.
data Label = Label
    { lcolour :: [Colour]
      -- ^ The colours of the route.
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

-- | Convert a graph back into a list of edges.
toList :: Enum a => Graph a -> [(a, a, Label)]
toList gr = [(toEnum n1, toEnum n2, l) | (n1, n2, l) <- G.labEdges (graph gr), n1 < n2]


-------------------------------------------------------------------------------
-- Nodes

-- | Get the neighbours of a node.
neighbours :: Enum a => a -> Graph a -> [a]
neighbours from gr = map toEnum $ G.suc (graph gr) (fromEnum from)


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
claimEdge :: Enum a => a -> a -> Colour -> Graph a -> Graph a
claimEdge = modlabel claim where
  claim l = Just l { lweight = 0 }

-- | Get the list of edges we own.
claimedEdges :: Enum a => Graph a -> [(a, a, Label)]
claimedEdges = filter (\(_, _, l) -> lweight l == 0) . toList

-- | Remove an edge from the graph. This is a simple way of indicating
-- that someone else has claimed it. If the edge has multiple colours,
-- only the given colour is removed; otherwise the edge is removed
-- entirely.
loseEdge :: Enum a => a -> a -> Colour -> Graph a -> Graph a
loseEdge from to colour = modlabel lose from to colour where
  lose l = case lcolour l of
    [_] -> Nothing
    cs  -> Just l { lcolour = let (eq,neq) = partition (==colour) cs in tail eq ++ neq }


-------------------------------------------------------------------------------
-- Paths

-- | Get the shortest path between two nodes, if one exists. If two
-- paths have the same weight, but one has fewer edges, that one will
-- be preferred. Routes which are already owned are not included, so
-- the \"path\" may be disconnected.
shortestPath :: Enum a => a -> a -> Graph a -> Maybe [(a, a, Label)]
shortestPath from to gr
    | null sp   = Nothing
    | otherwise = Just (go sp)
  where
    -- the shortest path
    sp = G.sp (fromEnum from) (fromEnum to) (graph gr2)

    -- convert the FGL path into the edge format we use.
    go (n1:n2:rest) =
      let lbl = edgelbl n1 n2
          therest = go (n2:rest)
      in if lweight lbl == 0
         then therest
         else (toEnum n1, toEnum n2, lbl) : therest
    go _ = []

    -- this use of 'fromJust' is safe, because 'G.sp' only produces a
    -- valid path.
    edgelbl n1 n2 = fromJust (edgeFromTo (toEnum n1) (toEnum n2) gr)

    -- construct a new graph which is the same, but all edge weights
    -- have 1 added. This means that a single n-cost edge will be
    -- preferred over a multi-edge path summing to n.
    gr2 = modgraph (\(n1, n2, l) -> Just (n1, n2, l+1)) gr

-- | Get the total number of cards needed to build a path.
pathCost :: Num n => [(a, a, Label)] -> n
pathCost = fromIntegral . sum . map (\(_, _, l) -> lweight l)

-- | Get the score gained by building a path.
pathScore :: Num n => [(a, a, Label)] -> n
pathScore = sum . mapMaybe (\(_, _, l) -> routeScore (lweight l))


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
modgraph f = fromList . mapMaybe f . toList

-- | Modify a single edge in a graph.
modlabel :: Enum a => (Label -> Maybe Label) -> a -> a -> Colour -> Graph a -> Graph a
modlabel f from to colour = modgraph go where
  go (n1, n2, l)
    | colour `elem` lcolour l && (check n1 n2 || check n2 n1)
      = (\l' -> (n1, n2, l')) <$> f l
    | otherwise = Just (n1, n2, l)

  check n1 n2 = fromEnum n1 == fromEnum from && fromEnum n2 == fromEnum to
