module AI
  (-- * AI player state
    State(..)
  , newState
    -- * Plans
  , planTickets
  , replanTickets
  ) where

import Control.Arrow ((&&&))
import Control.Monad (filterM)
import Data.List (foldl', sortOn)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Ord (Down(..))

import Graph

-------------------------------------------------------------------------------
-- AI player state

data State a = State
  { completedTickets :: [(a, a, Int)]
  -- ^ Successfully completed tickets
  , missedTickets :: [(a, a, Int)]
  -- ^ Tickets which are impossible to complete (want to keep this
  -- list small!)
  , pendingTickets :: [(a, a, Int)]
  -- ^ Tickets still being built.
  , plan :: [(a, a, Label)]
  -- ^ Planned edges to build. In the case of parallel edges, any one
  -- is acceptable.
  , remainingTrains :: Int
  -- ^ The number of trains remaining to build with, used to discard
  -- plans which are too close for comfort.
  , world :: Graph a
  -- ^ The world map, with our edges at cost 0 and other players'
  -- edges removed.
  }
  deriving Show

-- | Construct a new game state.
newState :: Int -> Graph a -> State a
newState = State [] [] [] []


-------------------------------------------------------------------------------
-- Plans

-- | Given a list of tickets, plan a rail network and decide which to
-- keep.
planTickets :: Enum a
  => State a
  -- ^ The AI state. The pending tickets are incorporated into the new
  -- plan.
  -> NonEmpty (a, a, Int)
  -- ^ The new tickets: at least one MUST be kept.
  -> (NonEmpty (a, a, Int), [(a, a, Label)])
planTickets ai tickets = head . sortOn cmp $ planTickets' ai tickets where
  cmp = (tclass &&& Down . pathScore) . snd

  -- split up path lengths into fairly coarse-grained categories
  tclass path | pathCost path < 3 * remainingTrains ai `div` 4 = 0
              | pathCost path <     remainingTrains ai         = 1
              | otherwise = 2

-- | Replan the rail network, given the currently-pending tickets.
replanTickets :: Enum a => State a -> [(a, a, Label)]
replanTickets ai = case pendingTickets ai of
  (t:ts) -> snd (planTickets ai { pendingTickets = ts } (t:|[]))
  []     -> []

-- | Come up with a variety of plans. This produces one plan for every
-- element of the powerset of the new tickets (barring the empty
-- list).
--
-- Plans are ordering-dependent, so for best variety, different
-- orderings of the tickets should be considered.
planTickets' :: Enum a
  => State a
  -> NonEmpty (a, a, Int)
  -> [(NonEmpty (a, a, Int), [(a, a, Label)])]
planTickets' ai = map (\(t:ts) -> (t:|ts, fst $ doplan (t:ts))) . powerset where
  -- the list monad is a kind of magic
  powerset = init . filterM (const [True, False]) . Data.List.NonEmpty.toList

  -- the mandatory tickets are taken from the AI state, in reverse
  -- order. To take advantage of this, have the final element of the
  -- list be the long route. This will tend to encourage a
  -- \"backbone\" style, where the long route forms a main route which
  -- others branch off from.
  tickets0 = reverse (pendingTickets ai)

  -- the initial plan is a plan just incorporating the tickets we
  -- currently have, with none of the new ones.
  plan0 = doplan []

  -- for each ticket in turn, compute the shortest path and add it to
  -- the network.
  doplan ts =
    let base = if null ts then ([], world ai) else plan0
        stepPlan (p, w) (destA, destB, _) =
          let path = shortestPath destA destB w
              p' = [ (a, b, l) | (a, b, l) <- path
                               , not $ (a, b) `elem'` p
                   ]
              claim (a, b, l) = claimEdge a b (head $ lcolour l)
          in (p ++ p', foldl' (flip claim) w path)
    in foldl' stepPlan base (tickets0 ++ ts)

  -- like 'elem', but using 'Enum' instead of 'Eq', and
  -- order-agnostic.
  elem' x@(a1, b1) ((a2, b2, _):xs) =
    ((fromEnum a1, fromEnum b1) == (fromEnum a2, fromEnum b2)) ||
    ((fromEnum b1, fromEnum a1) == (fromEnum a2, fromEnum b2)) ||
    elem' x xs
  elem' _ [] = False
