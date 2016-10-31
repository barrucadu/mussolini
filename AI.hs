module AI
  (-- * AI player state
    State(..)
  , newState
    -- * AI
  , Move(..)
  , suggest
  , suggestDraw
  , suggestRoute
    -- * Plans
  , Ticket(..)
  , planTickets
  , replanTickets
  , updateTickets
  , inPlan
    -- * Actions
  , setCards
  , draw
  , discard
  , claim
  , claimSingle
  , enemyClaim
  , enemyClaimSingle
  ) where

import Control.Monad (filterM)
import Data.List (foldl', partition, sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.Ord (Down(..))

import Graph

-------------------------------------------------------------------------------
-- AI player state

data State a = State
  { completedTickets :: [Ticket a]
  -- ^ Successfully completed tickets
  , missedTickets :: [Ticket a]
  -- ^ Tickets which are impossible to complete (want to keep this
  -- list small!)
  , pendingTickets :: [Ticket a]
  -- ^ Tickets still being built.
  , plan :: [(a, a, Label)]
  -- ^ Planned edges to build. In the case of parallel edges, any one
  -- is acceptable.
  , hand :: Map Colour Int
  -- ^ The current hand.
  , ontable :: Map Colour Int
  -- ^ The currently-visible cards.
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
newState = State [] [] [] [] M.empty M.empty


-------------------------------------------------------------------------------
-- AI

-- | A move that can be performed by a player.
data Move a
  = DrawLocomotiveCard
  -- ^ Draw a single visible locomotive card.
  | DrawCards (Maybe Colour) (Maybe Colour)
  -- ^ Draw cards, possibly from the deck.
  | ClaimRoute a a Colour
  -- ^ Claim a route, if the cards are in hand.
  | DrawTickets
  -- ^ Draw new ticket cards.
  deriving (Eq, Show)

-- | Suggest the next move.
suggest :: Enum a => State a -> Move a
suggest ai | drawLocomotive = suggestedDraw
           | claimRoute     = fromJust planned
           | drawTickets    = DrawTickets
           | shouldBuild    = fromJust routes
           | otherwise      = suggestedDraw
  where
    drawLocomotive = case suggestedDraw of DrawLocomotiveCard -> True; _ -> False
    claimRoute     = isJust planned
    drawTickets    = null (pendingTickets ai) &&
                     remainingTrains ai > minRemainingTrains &&
                     length (missedTickets ai) < maxMissedTickets
    shouldBuild    = isJust routes && null (plan ai)

    suggestedDraw = suggestDraw ai
    planned = suggestRoute True ai
    routes  = suggestRoute False ai

    -- if the number of remaining trains is below this point, don't
    -- draw a new ticket.
    minRemainingTrains = 10

    -- if the number of missed tickets is greater than this point,
    -- don't draw a new ticket.
    maxMissedTickets = 3

-- | Suggest a card to draw, with preference towards locomotives.
suggestDraw :: State a -> Move a
suggestDraw ai
    | M.findWithDefault 0 Special (ontable ai) > 0 = DrawLocomotiveCard
    | otherwise = DrawCards colour1 colour2
  where
    -- the needed colours
    neededColours = sortOn snd [ (c, i) | c <- [minBound..maxBound]
                                        , let i = nInPlan c - nInHand c
                                        , i > 0
                               ]

    -- how many times a colour shows up in the plan
    nInPlan c = sum $ map (wval c) (plan ai) where
      wval Special (_, _, l) = lweight l
      wval c (_, _, l)
        | c `elem` lcolour l || Special `elem` lcolour l = lweight l - llocos l
        | otherwise = 0

    -- how many times a colour shows up in the hand
    nInHand c = M.findWithDefault 0 c (hand ai)

    -- if a colour is on the table
    hasColour c i = M.findWithDefault 0 c (ontable ai) >= i

    -- the colours to claim.
    (colour1, colour2) = case filter (\(c,_) -> hasColour c 1) neededColours of
      ((colour, n):rest)
        | n >= 2 && hasColour colour 2 -> (Just colour, Just colour)
        | otherwise -> (Just colour, fst <$> listToMaybe rest)
      [] -> (Nothing, Nothing)

-- | Suggest a route to build, if possible.
suggestRoute :: Enum a => Bool -> State a -> Maybe (Move a)
suggestRoute onlyPlanned ai = listToMaybe routes where
  -- all routes.
  allRoutes | onlyPlanned = plan
            | otherwise   = toList . world

  -- routes which can be claimed.
  routes =
    [ ClaimRoute from to colour | (from, to, label) <- allRoutes ai
                                , colour <- lcolour label
                                , canClaim colour (llocos label) (lweight label)
    ]

  -- check if there are enough locomotives and trains in hand to claim the route.
  canClaim colour locos weight =
    let numLocos = M.findWithDefault 0 Special (hand ai)
        hand' = M.update (\n -> if n <= locos then Nothing else Just (n-locos)) Special (hand ai)
        remainingLocos = M.findWithDefault 0 Special hand'
        trains
          | colour == Special = listToMaybe . sortOn (Down . snd) $ M.assocs hand'
          | otherwise = Just (colour, M.findWithDefault 0 colour hand')
    in case trains of
      Just (Special, _)  -> numLocos >= weight
      Just (_, num) -> numLocos >= locos && num + remainingLocos >= weight - locos
      Nothing -> False


-------------------------------------------------------------------------------
-- Plans

-- | A 'Ticket' is a pair of destinations and a point value.
data Ticket a = Ticket { tfrom :: a, tto :: a, tvalue :: Int }
  deriving (Eq, Show)

-- | Given a list of tickets, plan a rail network and decide which to
-- keep.
planTickets :: Enum a
  => NonEmpty (Ticket a)
  -- ^ The new tickets: at least one MUST be kept.
  -> State a
  -- ^ The AI state. The pending tickets are incorporated into the new
  -- plan.
  -> (NonEmpty (Ticket a), [(a, a, Label)])
planTickets tickets ai = fst2 . head . sortOn cmp $ planTickets' ai tickets where
  cmp (_, path, waste) = (waste, tclass path, Down $ pathScore path) :: (Int, Int, Down Int)
  fst2 (a, b, _) = (a, b)

  -- split up path lengths into fairly coarse-grained categories
  tclass path | pathCost path < 3 * remainingTrains ai `div` 4 = 0
              | pathCost path <     remainingTrains ai         = 1
              | otherwise = 2


-- | Replan the rail network, given the currently-pending tickets.
replanTickets :: Enum a => State a -> [(a, a, Label)]
replanTickets ai = case pendingTickets ai of
  (t:ts) -> snd (planTickets (t:|[]) ai { pendingTickets = ts })
  []     -> []

-- | Come up with a variety of plans. This produces one plan for every
-- element of the powerset of the new tickets (barring the empty
-- list). The third element of the result tuple is the unreadable
-- ticket value (this wants to be minimised!)
--
-- Plans are ordering-dependent, so for best variety, different
-- orderings of the tickets should be considered.
planTickets' :: Enum a
  => State a
  -> NonEmpty (Ticket a)
  -> [(NonEmpty (Ticket a), [(a, a, Label)], Int)]
planTickets' ai ts0
    | null reachable = [(leastBad:|[], fst plan0, tvalue leastBad)]
    | otherwise = map (\(r:rs) -> (r:|rs, fst $ doplan (r:rs), 0)) (powerset reachable)
  where
    -- the reachable tickets
    reachable = filter isReachable (L.toList ts0)

    -- the least-bad unreachable ticket. As at least one must be kept,
    -- this one is picked if none are reachable. The use of 'head' is
    -- safe here, as this value is only inspected if the 'reachable'
    -- list is empty, which means there must be at least one
    -- unreachable ticket.
    leastBad = head . sortOn tvalue . filter (not . isReachable) . L.toList $ ts0

    -- check if a ticket is reachable
    isReachable t = isJust $ shortestPath (tfrom t) (tto t) (world ai)

    -- the list monad is a kind of magic
    powerset = init . filterM (const [True, False])

    -- the mandatory tickets are taken from the AI state, in reverse
    -- order. To take advantage of this, have the final element of the
    -- list be the long route. This will tend to encourage a
    -- \"backbone\" style, where the long route forms a main route
    -- which others branch off from.
    tickets0 = filter isReachable . reverse $ pendingTickets ai

    -- the initial plan is a plan just incorporating the tickets we
    -- currently have, with none of the new ones.
    plan0 = doplan []

    -- for each ticket in turn, compute the shortest path and add it
    -- to the network. This must only be called with reachable
    -- tickets.
    doplan ts =
      let base = if null ts then ([], world ai) else plan0
          stepPlan (p, w) t =
            let path = fromJust $ shortestPath (tfrom t) (tto t) w
                p' = [ (a, b, l) | (a, b, l) <- path
                                 , not $ inPlan a b p
                     ]
                claimE (a, b, l) = claimEdge a b (head $ lcolour l)
            in (p ++ p', foldl' (flip claimE) w path)
      in foldl' stepPlan base (tickets0 ++ ts)

-- | Check if any pending tickets have been completed or blocked, and
-- update as appropriate. If a ticket is removed from the pending
-- list, the plan is recomputed.
updateTickets :: Enum a => State a -> State a
updateTickets ai0 = replan ai0
    { completedTickets = newComplete ++ completedTickets ai0
    , pendingTickets   = newPending
    , missedTickets    = newMissed ++ missedTickets ai0
    }
  where
    replan ai | length newPending == length (pendingTickets ai0) = ai
              | otherwise = ai { plan = replanTickets ai }

    (newComplete, pending') = partition isComplete (pendingTickets ai0)
    (newMissed, newPending) = partition isMissed pending'

    isComplete t = (null <$> shortestPath (tfrom t) (tto t) (world ai0)) == Just True
    isMissed   t = isNothing $ shortestPath (tfrom t) (tto t) (world ai0)

-- | Check if an edge is in a plan.
inPlan :: Enum a => a -> a -> [(a, a, label)] -> Bool
inPlan a1 b1 = any go where
  go (a2, b2, _) = (fromEnum a2, fromEnum b2) `elem` cmps
  cmps = [(fromEnum a1, fromEnum b1), (fromEnum b1, fromEnum a1)]


-------------------------------------------------------------------------------
-- Actions

-- | Set the visible cards on the table.
setCards :: [Colour] -> State a -> State a
setCards colours ai = ai { ontable = M.fromList [(c, count c) | c <- colours] } where
  count c = length $ filter (==c) colours

-- | Draw new cards.
draw :: [Colour] -> State a -> State a
draw cards ai = ai { hand = foldl' (flip $ M.alter go) (hand ai) cards } where
  go (Just i) = Just (i+1)
  go Nothing  = Just 1

-- | Discard cards.
discard :: [Colour] -> State a -> State a
discard cards ai = ai { hand = foldl' (flip $ M.update go) (hand ai) cards } where
  go i | i <= 1    = Nothing
       | otherwise = Just (i-1)

-- | Claim a route. This removes the route from the plan (if it's
-- present), updates the pending tickets if necessary, and recomputes
-- the plan if this completes a ticket.
--
-- Make sure to 'discard' the needed cards! This function doesn't do
-- so automatically, as locomotives give rise to multiple ways to pay
-- for the same route in some cases.
claim :: Enum a => a -> a -> Colour -> State a -> State a
claim from to colour ai = updateTickets ai
  { plan  = filter (not . inPlan from to . (:[])) (plan ai)
  , world = claimEdge from to colour (world ai)
  }

-- | Helper for 'claim' for the case where there is only a single
-- (remaining) route between the two places.
--
-- If there are multiple colours, this does not modify the state of
-- the world.
claimSingle :: Enum a => a -> a -> State a -> State a
claimSingle from to ai = case colour of
    (col:_) -> claim from to col ai
    _ -> ai
  where
    colour = maybe [] lcolour $ edgeFromTo from to (world ai)

-- | Have an enemy claim a route. This checks if any tickets have been
-- blocked, removes the route from the plan, and recomputes the plan
-- if necessary.
enemyClaim :: Enum a => a -> a -> Colour -> State a -> State a
enemyClaim from to colour ai = updateTickets ai
    { plan  = newPlan
    , world = newWorld
    }
  where
    newWorld = loseEdge from to colour (world ai)
    newPlan | inPlan from to (plan ai) = replanTickets ai { world = newWorld }
            | otherwise = plan ai

-- | Helper for 'enemyClaim' for the case where there is only a single
-- (remaining) route between the two places.
--
-- If there are multiple colours, this does not modify the state of
-- the world.
enemyClaimSingle :: Enum a => a -> a -> State a -> State a
enemyClaimSingle from to ai = case colour of
    (col:_) -> enemyClaim from to col ai
    _ -> ai
  where
    colour = maybe [] lcolour $ edgeFromTo from to (world ai)
