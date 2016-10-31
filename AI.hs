module AI
  (-- * AI player state
    State(..)
  , newState
    -- * AI
  , Move(..)
  , suggest
    -- * Plans
  , planTickets
  , replanTickets
  , inPlan
    -- * Actions
  , draw
  , discard
  , claim
  , enemyClaim
  ) where

import Control.Arrow ((&&&))
import Control.Monad (filterM)
import Data.List (foldl', sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
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
  , hand :: Map (Maybe Colour) Int
  -- ^ The current hand. @Nothing@ indicates a locomotive.
  , ontable :: Map (Maybe Colour) Int
  -- ^ The currently-visible cards. @Nothing@ indicates a locomotive.
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
  | ClaimRoute a a (Maybe Colour)
  -- ^ Claim a route, if the cards are in hand.
  | DrawTickets
  -- ^ Draw new ticket cards.
  deriving (Eq, Show)

-- | Suggest the next move.
suggest :: Enum a => State a -> Move a
suggest ai | drawLocomotive = DrawLocomotiveCard
           | claimRoute     = head planned
           | drawTickets    = DrawTickets
           | canBuild       = head routes
           | otherwise      = DrawCards colour1 colour2
  where
    drawLocomotive = M.lookup Nothing (ontable ai) `notElem` [Nothing, Just 0]
    claimRoute     = not (null planned)
    drawTickets    = null (pendingTickets ai) &&
                     remainingTrains ai > minRemainingTrains &&
                     length (missedTickets ai) < maxMissedTickets
    canBuild       = not (null routes)

    -- routes that can be claimed.
    planned =
      [ ClaimRoute from to colour | (from, to, label) <- plan ai
                                  , colour <- lcolour label
                                  , canClaim colour (llocos label) (lweight label)
      ]
    routes =
      [ ClaimRoute from to colour | (from, to, label) <- toList (world ai)
                                  , colour <- lcolour label
                                  , canClaim colour (llocos label) (lweight label)
      ]

    canClaim colour locos weight =
      let numLocos = M.findWithDefault 0 Nothing (hand ai)
          hand' = M.update (\n -> if n <= locos then Nothing else Just (n-locos)) Nothing (hand ai)
          remainingLocos = M.findWithDefault 0 Nothing hand'
          trains = case colour of
            Just col -> Just (colour, M.findWithDefault 0 colour hand')
            Nothing  -> listToMaybe . sortOn (Down . snd) $ M.assocs hand'
      in case trains of
        Just (Just col, num) -> numLocos >= locos && num + remainingLocos >= weight - locos
        Just (Nothing, num)  -> numLocos >= weight
        Nothing -> False

    -- the colours to claim, favour taking pairs.
    (colour1, colour2) = case filter (\(c,_) -> hasColour c 1) neededColours of
      ((colour, n):rest)
        | n >= 2 && hasColour colour 2 -> (Just colour, Just colour)
        | otherwise -> (Just colour, fst <$> listToMaybe rest)
      [] ->
        let pairs  = filter ((>=2) . snd) $ M.assocs (ontable ai)
            colour = listToMaybe pairs >>= fst
        in (colour, colour)

    neededColours = sortOn snd [(c, i) | c <- [minBound..maxBound], let i = nInPlan c, i > 0]
    nInPlan c = sum $ map (wval c) (plan ai)
    wval c (_, _, l) | Just c `elem` lcolour l = lweight l
                     | otherwise = 0
    hasColour c i = maybe False (>i) (M.lookup (Just c) $ ontable ai)

    -- if the number of remaining trains is below this point, don't
    -- draw a new ticket.
    minRemainingTrains = 10

    -- if the number of missed tickets is greater than this point,
    -- don't draw a new ticket.
    maxMissedTickets = 3


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
  powerset = init . filterM (const [True, False]) . L.toList

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
                               , not $ inPlan a b p
                   ]
              claim (a, b, l) = claimEdge a b (head $ lcolour l)
          in (p ++ p', foldl' (flip claim) w path)
    in foldl' stepPlan base (tickets0 ++ ts)

-- | Check if an edge is in a plan.
inPlan :: Enum a => a -> a -> [(a, a, label)] -> Bool
inPlan a1 b1 = any go where
  go (a2, b2, _) = (fromEnum a2, fromEnum b2) `elem` cmps
  cmps = [(fromEnum a1, fromEnum b1), (fromEnum b1, fromEnum a1)]

-------------------------------------------------------------------------------
-- Actions

-- | Draw new cards.
draw :: State a -> [Maybe Colour] -> State a
draw ai cards = ai { hand = foldl' (flip $ M.alter go) (hand ai) cards } where
  go (Just i) = Just (i+1)
  go Nothing  = Just 1

-- | Discard cards.
discard :: State a -> [Maybe Colour] -> State a
discard ai cards = ai { hand = foldl' (flip $ M.update go) (hand ai) cards } where
  go i | i <= 1    = Nothing
       | otherwise = Just (i-1)

-- | Claim a route. This removes the route from the plan (if it's
-- present), but does not recompute the plan if it isn't, which may be
-- desirable in some cases.
--
-- Make sure to 'discard' the needed cards! This function doesn't do
-- so automatically, as locomotives give rise to multiple ways to pay
-- for the same route in some cases.
claim :: Enum a => State a -> a -> a -> Maybe Colour -> State a
claim ai from to colour = ai { plan = newPlan, world = newWorld } where
  newWorld = claimEdge from to colour (world ai)
  newPlan = filter go (plan ai)
  go p = not $ inPlan from to [p]

-- | Have an enemy claim a route. If the route is in the plan, it is
-- recomputed.
enemyClaim :: Enum a => State a -> a -> a -> Maybe Colour -> State a
enemyClaim ai from to colour = ai { plan = newPlan, world = newWorld } where
  newWorld = loseEdge from to colour (world ai)
  newPlan | inPlan from to (plan ai) = replanTickets ai { world = newWorld }
          | otherwise = plan ai
